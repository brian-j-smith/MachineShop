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
#' @seealso \code{\link[lars]{lars}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package lars to run
#'
#' fit(sale_amount ~ ., data = ICHomes, model = LARSModel)
#' }
#'
LARSModel <- function(
  type = c("lasso", "lar", "forward.stagewise", "stepwise"), trace = FALSE,
  normalize = TRUE, intercept = TRUE, step = NULL, use.Gram = TRUE
) {

  type <- match.arg(type)

  MLModel(
    name = "LARSModel",
    label = "Least Angle Regression",
    packages = "lars",
    response_types = "numeric",
    predictor_encoding = "model.matrix",
    params = new_params(environment()),
    gridinfo = new_gridinfo(
      param = "step",
      get_values = c(
        function(n, data, ...) seq_nvars(data, LARSModel, n)
      )
    ),
    fit = function(formula, data, weights, step = NULL, ...) {
      throw(check_equal_weights(weights))
      x <- model.matrix(data, intercept = FALSE)
      y <- response(data)
      if (is.null(step)) {
        model_fit <- lars::lars(x, y, ...)
        model_fit$step <- length(model_fit$df)
      } else {
        model_fit <- lars::lars(x, y, max.steps = ceiling(step), ...)
        model_fit$step <- step
      }
      model_fit
    },
    predict = function(object, newdata, ...) {
      newx <- model.matrix(newdata, intercept = FALSE)
      predict(object, newx = newx, s = object$step, type = "fit")$fit
    }
  )

}

MLModelFunction(LARSModel) <- NULL
