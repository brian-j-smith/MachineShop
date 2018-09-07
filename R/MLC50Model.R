#' C5.0 Decision Trees and Rule-Based Model
#' 
#' Fit classification tree models or rule-based models using Quinlan's C5.0
#' algorithm.
#'
#' @param trials integer number of boosting iterations. A value of one indicates
#' that a single model is used.
#' @param rules logical indicating whether the tree is decomposed into a
#' rule-based model
#' @param control list of control parameters.
#'
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[C50]{C5.0}}, \code{\link{fit}}, \code{\link{resample}}
#'
C50Model <- function(trials = NULL, rules = NULL, control = NULL)
  {
  MLModel(
    name = "C50Model",
    packages = "C50",
    types = "factor",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      mfit <- C50::C5.0(formula, data = data, weights = weights, ...)
      mfit$y <- response(formula, data)
      asMLModelFit(mfit, "C50Fit", C50Model(...))
    },
    predict = function(object, newdata, ...) {
      predict(asParentFit(object), newdata = newdata, type = "prob")
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      C50::C5imp(object, ...)
    }
  )
}
