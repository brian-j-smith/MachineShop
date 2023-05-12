#' Parsnip Model
#'
#' Convert a model specification from the \pkg{parsnip} package to one that can
#' be used with the \pkg{MachineShop} package.
#'
#' @param object \link[parsnip:model_spec]{model specification} from the
#'   \pkg{parsnip} package.
#' @param ... tuning parameters with which to update \code{object}.
#'
#' @return \code{ParsnipModel} class object that inherits from \code{MLModel}.
#'
#' @seealso \code{\link{as.MLModel}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package parsnip to run
#'
#' prsp_model <- parsnip::linear_reg(engine = "glmnet")
#'
#' model <- ParsnipModel(prsp_model, penalty = 1, mixture = 1)
#' model
#'
#' model_fit <- fit(sale_amount ~ ., data = ICHomes, model = model)
#' predict(model_fit)
#' }
#'
ParsnipModel <- function(object, ...) {

  throw(check_packages("parsnip"))

  if (missing(object)) {
    object <- parsnip::null_model() %>%
      parsnip::set_engine("parsnip")
    object$mode <- "unknown"
  } else {
    stopifnot(is(object, "model_spec"))
    object <- update(object, ...)
  }

  modes <- list("censored regression" = "Surv",
                "classification" = "factor",
                "regression" = c("matrix", "numeric"))
  modes$unknown <- sort(unlist(modes, use.names = FALSE))
  throw(check_assignment(mode, check_match(object$mode, names(modes))))

  new("ParsnipModel", MLModel(

    name = "ParsnipModel",
    label = sprintf("%s(mode = \"%s\", engine = \"%s\")",
                    class1(object), object$mode, object$engine),
    packages = parsnip::required_pkgs(object),
    response_types = modes[[object$mode]],
    predictor_encoding = "model.frame",
    params = list(object = object),

    fit = function(formula, data, weights, object, ...) {
      parsnip::fit(object, formula, data = as.data.frame(formula, data))
    },

    predict = function(object, newdata, times, .MachineShop, ...) {
      prsp_predict <- function(...) {
        predict(object, new_data = as.data.frame(newdata), ...)
      }
      model <- .MachineShop$model
      mode <- model@params$object$mode
      if (mode == "censored regression") {
        distr <- model@params$dist
        if (is(distr, "quosure")) distr <- rlang::quo_get_expr(distr)
        if (length(times)) {
          pred_list <- prsp_predict(type = "survival", eval_time = times)$.pred
          pred_vector <- do.call(rbind, pred_list)$.pred_survival
          pred <- matrix(pred_vector, ncol = length(times), byrow = TRUE)
          SurvProbs(pred, times = times, distr = distr)
        } else {
          SurvTimes(prsp_predict(type = "time")$.pred_time, distr = distr)
        }
      } else {
        pred <- prsp_predict(type = switch(mode, "classification" = "prob"))
        select <- grepl("^.pred", names(pred))
        names(pred) <- gsub("^.pred_", "", names(pred))
        as.matrix(pred[select])
      }
    }

  ))

}

MLModelFunction(ParsnipModel) <- NULL


update.ParsnipModel <- function(object, ...) {
  NextMethod(quote = FALSE)
}
