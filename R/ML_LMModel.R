#' Linear Models
#'
#' Fits linear models.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{matrix}, \code{numeric}}
#' }
#'
#' Further model details can be found in the source link below.
#'
#' In calls to \code{\link{varimp}} for \code{LModel}, numeric argument
#' \code{base} may be specified for the (negative) logarithmic transformation of
#' p-values [defaul: \code{exp(1)}].  Transformed p-values are automatically
#' scaled in the calculation of variable importance to range from 0 to 100.  To
#' obtain unscaled importance values, set \code{scale = FALSE}.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = LMModel)
#'
LMModel <- function() {

  MLModel(

    name = "LMModel",
    label = "Linear Model",
    packages = "stats",
    response_types = c("factor", "matrix", "numeric"),
    weights = TRUE,
    predictor_encoding = "model.matrix",
    params = new_params(environment()),

    fit = function(formula, data, weights, ...) {
      y <- response(data)
      data <- as.data.frame(data)
      if (is.factor(y)) {
        y_name <- response(formula)
        if (nlevels(y) == 2) {
          data[[y_name]] <- as.numeric(y) - 1
        } else {
          mm <- model.matrix(~ y - 1)
          colnames(mm) <- levels(y)
          data[[y_name]] <- mm
        }
      }
      stats::lm(formula, data = data, weights = weights, ...)
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata)
    },

    varimp = function(object, base = exp(1), ...) {
      varimp_pval(object, base = base)
    }

  )

}

MLModelFunction(LMModel) <- NULL
