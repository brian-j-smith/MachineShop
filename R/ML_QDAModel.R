#' Quadratic Discriminant Analysis Model
#'
#' Performs quadratic discriminant analysis.
#' 
#' @param prior prior probabilities of class membership if specified or the
#' class proportions in the training set otherwise.
#' @param method type of mean and variance estimator.
#' @param nu degrees of freedom for \code{method = "t"}.
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
#' @seealso \code{\link[MASS]{qda}}, \code{\link[MASS]{predict.qda}},
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = QDAModel())
#'
QDAModel <- function(prior = NULL, method = c("moment", "mle", "mve", "t"),
                     nu = 5,
                     use = c("plug-in", "predictive", "debiased", "looCV")) {
  
  method <- match.arg(method)
  use <- match.arg(use)
  
  MLModel(
    name = "QDAModel",
    label = "Quadratic Discriminant Analysis",
    packages = "MASS",
    types = "factor",
    params = params(environment()),
    design = "model.matrix",
    fit = function(formula, data, weights, use, ...) {
      assert_equal_weights(weights)
      modelfit <- MASS::qda(formula, data = data, ...)
      modelfit$use <- use
      modelfit
    },
    predict = function(object, newdata, prior = object$prior, ...) {
      predict(object, newdata = newdata, prior = prior,
              method = object$use)$posterior
    }
  )
  
}
