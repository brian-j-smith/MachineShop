#' Least Angle Regression, Lasso and Infinitesimal Forward Stagewise Models
#'
#' Fit variants of Lasso, and provide the entire sequence of coefficients and
#' fits, starting from zero to the least squares fit.
#' 
#' @param type model type.
#' @param trace logical indicating whether status information is printed during
#' the fitting process.
#' @param normalize whether to standardize each variable to have unit L2 norm. 
#' @param intercept whether to include an intercept in the model.
#' @param step algorithm step number to use for prediction.  May be a decimal
#' number indicating a fractional distance between steps.  If specified, the
#' maximum number of algorithm steps will be \code{ceiling(step)}; otherwise,
#' \code{step} will be set equal to the source package default maximum [default:
#' \code{max.steps}].
#' @param use.Gram whether to precompute the Gram matrix.
#'  
#' @details 
#' \describe{
#' \item{Response Types:}{\code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[lars]{lars}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = LARSModel)
#'
LARSModel <- function(type = c("lasso", "lar", "forward.stagewise", "stepwise"),
                      trace = FALSE, normalize = TRUE, intercept = TRUE,
                      step = NULL, use.Gram = TRUE) {
  
  type <- match.arg(type)

  MLModel(
    name = "LARSModel",
    label = "Least Angle Regression",
    packages = "lars",
    types = "numeric",
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, step = NULL, ...) {
      assert_equal_weights(weights)
      mf <- model.frame(formula, data, na.action = na.pass)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      if (is.null(step)) {
        modelfit <- lars::lars(x, y, ...)
        modelfit$step <- length(modelfit$df)
      } else {
        modelfit <- lars::lars(x, y, max.steps = ceiling(step), ...)
        modelfit$step <- step
      }
      modelfit
    },
    predict = function(object, newdata, fitbits, ...) {
      fo <- formula(fitbits)[-2]
      newmf <- model.frame(fo, newdata, na.action = na.pass)
      newx <- model.matrix(fo, newmf)[, -1, drop = FALSE]
      predict(object, newx = newx, s = object$step, type = "fit")$fit
    }
  )
  
}
