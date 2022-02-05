#' Recursive Partitioning and Regression Tree Models
#'
#' Fit an \code{rpart} model.
#'
#' @param minsplit minimum number of observations that must exist in a node in
#'   order for a split to be attempted.
#' @param minbucket minimum number of observations in any terminal node.
#' @param cp complexity parameter.
#' @param maxcompete number of competitor splits retained in the output.
#' @param maxsurrogate number of surrogate splits retained in the output.
#' @param usesurrogate how to use surrogates in the splitting process.
#' @param xval number of cross-validations.
#' @param surrogatestyle controls the selection of a best surrogate.
#' @param maxdepth maximum depth of any node of the final tree, with the root
#'   node counted as depth 0.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}, \code{numeric}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameter:}{
#'     \code{cp}
#'   }
#' }
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[rpart]{rpart}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested packages rpart and partykit to run
#'
#' fit(Species ~ ., data = iris, model = RPartModel)
#' }
#'
RPartModel <- function(
  minsplit = 20, minbucket = round(minsplit / 3), cp = 0.01, maxcompete = 4,
  maxsurrogate = 5, usesurrogate = 2, xval = 10, surrogatestyle = 0,
  maxdepth = 30
) {

  MLModel(

    name = "RPartModel",
    label = "Recursive Partitioning and Regression Trees",
    packages = c("rpart", "partykit"),
    response_types = c("factor", "numeric", "Surv"),
    weights = TRUE,
    predictor_encoding = "model.frame",
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = "cp",
      get_values = c(
        function(n, data, ...) {
          cptable <- fit(data, model = RPartModel(cp = 0))$cptable
          xerror_order <- order(cptable[, "xerror"] + cptable[, "xstd"])
          sort(head(cptable[xerror_order, "CP"], n))
        }
      )
    ),

    fit = function(formula, data, weights, ...) {
      method <- switch_class(response(data),
        "factor" = "class",
        "numeric" = "anova",
        "Surv" = "exp"
      )
      rpart::rpart(
        formula, data = as.data.frame(formula, data), weights = weights,
        na.action = na.pass, method = method, control = list(...)
      )
    },

    predict = function(object, newdata, model, ...) {
      y <- response(model)
      newdata <- as.data.frame(newdata)
      if (is.Surv(y)) {
        object <- partykit::as.party(object)
        fits <- predict(object, newdata = newdata, type = "prob")
        predict(y, fits, ...)
      } else {
        predict(object, newdata = newdata)
      }
    },

    varimp = function(object, ...) {
      object$variable.importance
    }

  )

}

MLModelFunction(RPartModel) <- NULL
