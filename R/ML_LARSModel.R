#' Least Angle Regression, Lasso and Infinitesimal Forward Stagewise Models
#'
#' Fit variants of Lasso, and provide the entire sequence of coefficients and
#' fits, starting from zero to the least squares fit.
#' 
#' @param type model type.
#' @param trace logical indicating whether status information is printed during
#'   the fitting process.
#' @param normalize whether to standardize each variable to have unit L2 norm. 
#' @param intercept whether to include an intercept in the model.
#' @param step algorithm step number to use for prediction.  May be a decimal
#'   number indicating a fractional distance between steps.  If specified, the
#'   maximum number of algorithm steps will be \code{ceiling(step)}; otherwise,
#'   \code{step} will be set equal to the source package default maximum
#'   [default: \code{max.steps}].
#' @param use.Gram whether to precompute the Gram matrix.
#'  
#' @details 
#' \describe{
#'   \item{Response Types:}{\code{numeric}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{step}
#'   }
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
#' fit(sale_amount ~ ., data = ICHomes, model = LARSModel)
#'
LARSModel <- function(type = c("lasso", "lar", "forward.stagewise", "stepwise"),
                      trace = FALSE, normalize = TRUE, intercept = TRUE,
                      step = NULL, use.Gram = TRUE) {
  
  type <- match.arg(type)

  MLModel(
    name = "LARSModel",
    label = "Least Angle Regression",
    packages = "lars",
    response_types = "numeric",
    predictor_encoding = "model.matrix",
    params = params(environment()),
    grid = function(x, length, ...) {
      list(
        step = seq_nvars(x, LARSModel, length)
      )
    },
    fit = function(formula, data, weights, step = NULL, ...) {
      assert_equal_weights(weights)
      x <- model.matrix(data, intercept = FALSE)
      y <- response(data)
      if (is.null(step)) {
        modelfit <- lars::lars(x, y, ...)
        modelfit$step <- length(modelfit$df)
      } else {
        modelfit <- lars::lars(x, y, max.steps = ceiling(step), ...)
        modelfit$step <- step
      }
      modelfit
    },
    predict = function(object, newdata, ...) {
      newx <- model.matrix(newdata, intercept = FALSE)
      predict(object, newx = newx, s = object$step, type = "fit")$fit
    }
  )
  
}

MLModelFunction(LARSModel) <- NULL
