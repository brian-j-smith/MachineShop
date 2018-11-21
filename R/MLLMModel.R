#' Linear Models
#'
#' Fits linear models.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{matrix}, \code{numeric}}
#' }
#' 
#' Further model details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[stats]{lm}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = LMModel())
#'
LMModel <- function() {
  MLModel(
    name = "LMModel",
    packages = "stats",
    types = c("factor", "matrix", "numeric"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      y <- response(formula, data)
      if (is_response(y, "binary")) {
        y <- y == levels(y)[2]
      } else if (is_response(y, "factor")) {
        varname <- response(terms(formula))
        mm <- model.matrix(~ y - 1)
        colnames(mm) <- levels(y)
        data[[varname]] <- mm
        formula[[2]] <- as.symbol(varname)
      }
      args <- list(formula = formula, data = data, ...)
      if (is_response(y, "numeric")) {
        args$weights <- weights
      } else if (!all(weights == 1)) {
        warning("weights are unsupported and will be ignored")
      }
      do.call(stats::lm, args)
    },
    predict = function(object, newdata, ...) {
      predict(unMLModelFit(object), newdata = newdata)
    },
    varimp = function(object, ...) {
      pchisq(coef(object)^2 / diag(vcov(object)), 1)
    }
  )
}
