#' Linear Discriminant Analysis Model
#'
#' Performs linear discriminant analysis.
#' 
#' @param prior prior probabilities of class membership if specified or the
#' class proportions in the training set otherwise.
#' @param tol tolerance for the determination of singular matrices.
#' @param method type of mean and variance estimator.
#' @param nu degrees of freedom for \code{method = "t"}.
#' @param dimen dimension of the space to use for prediction.
#' @param use type of parameter estimation to use for prediction.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}}
#' }
#' 
#' The \code{\link{predict}} function for this model additionally accepts the
#' following argument.
#' \describe{
#' \item{\code{prior}}{prior class membership probabilities for prediction data
#' if different from the training set.}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}},
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = LDAModel())
#'
LDAModel <- function(prior = NULL, tol = 1e-4,
                     method = c("moment", "mle", "mve", "t"), nu = 5,
                     dimen = NULL,
                     use = c("plug-in", "debiased", "predictive")) {
  
  method <- match.arg(method)
  use <- match.arg(use)
  
  MLModel(
    name = "LDAModel",
    label = "Linear Discriminant Analysis",
    packages = "MASS",
    types = "factor",
    params = params(environment()),
    grid = function(x, length, ...) {
      list(
        dimen = 1:min(nlevels(response(x)) - 1, nvars(x, LDAModel))
      )
    },
    design = "model.matrix",
    fit = function(formula, data, weights, dimen, use, ...) {
      assert_equal_weights(weights)
      modelfit <- MASS::lda(formula, data = data, ...)
      modelfit$dimen <- if (missing(dimen)) length(modelfit$svd) else dimen
      modelfit$use <- use
      modelfit
    },
    predict = function(object, newdata, prior = object$prior, ...) {
      predict(object, newdata = newdata, prior = prior, dimen = object$dimen,
              method = object$use)$posterior
    }
  )
  
}
