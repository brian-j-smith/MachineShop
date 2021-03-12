#' Linear Discriminant Analysis Model
#'
#' Performs linear discriminant analysis.
#'
#' @param prior prior probabilities of class membership if specified or the
#'   class proportions in the training set otherwise.
#' @param tol tolerance for the determination of singular matrices.
#' @param method type of mean and variance estimator.
#' @param nu degrees of freedom for \code{method = "t"}.
#' @param dimen dimension of the space to use for prediction.
#' @param use type of parameter estimation to use for prediction.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{dimen}
#'   }
#' }
#'
#' The \code{\link{predict}} function for this model additionally accepts the
#' following argument.
#' \describe{
#'   \item{\code{prior}}{prior class membership probabilities for prediction
#'     data if different from the training set.}
#' }
#'
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}},
#' \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(Species ~ ., data = iris, model = LDAModel)
#'
LDAModel <- function(
  prior = NULL, tol = 1e-4, method = c("moment", "mle", "mve", "t"), nu = 5,
  dimen = NULL, use = c("plug-in", "debiased", "predictive")
) {

  method <- match.arg(method)
  use <- match.arg(use)

  MLModel(
    name = "LDAModel",
    label = "Linear Discriminant Analysis",
    packages = "MASS",
    response_types = "factor",
    predictor_encoding = "model.matrix",
    params = params(environment()),
    gridinfo = new_gridinfo(
      param = "dimen",
      values = c(
        function(n, data, ...) {
          1:min(nlevels(response(data)) - 1, nvars(data, LDAModel), n)
        }
      )
    ),
    fit = function(formula, data, weights, dimen, use, ...) {
      assert_equal_weights(weights)
      model_fit <- MASS::lda(formula, data = as.data.frame(data), ...)
      model_fit$dimen <- if (missing(dimen)) length(model_fit$svd) else dimen
      model_fit$use <- use
      model_fit
    },
    predict = function(object, newdata, prior = object$prior, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, prior = prior, dimen = object$dimen,
              method = object$use)$posterior
    }
  )

}

MLModelFunction(LDAModel) <- NULL
