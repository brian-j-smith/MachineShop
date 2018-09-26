#' C5.0 Decision Trees and Rule-Based Model
#' 
#' Fit classification tree models or rule-based models using Quinlan's C5.0
#' algorithm.
#'
#' @param trials integer number of boosting iterations.
#' @param rules logical indicating whether to decompose the tree into a
#' rule-based model.
#' @param subset logical indicating whether the model should evaluate groups of
#' discrete predictors for splits.
#' @param bands integer between 2 and 1000.
#' @param winnow logical indicating use of predictor winnowing (i.e. feature
#' selection).
#' @param noGlobalPruning logical indicating a final, global pruning step to
#' simplify the tree.
#' @param CF number in (0, 1) for the confidence factor.
#' @param minCases integer for the smallest number of samples that must be put
#' in at least two of the splits.
#' @param fuzzyThreshold logical indicating whether to evaluate possible
#' advanced splits of the data.
#' @param sample value between (0, 0.999) that specifies the random proportion
#' of data to use in training the model.
#' @param earlyStopping logical indicating whether the internal method for
#' stopping boosting should be used.
#'
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}}
#' }
#' 
#' Latter arguments are passed to \code{\link[C50]{C5.0Control}}.
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[C50]{C5.0}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#'
C50Model <- function(trials = NULL, rules = NULL, subset = NULL, bands = NULL,
                     winnow = NULL, noGlobalPruning = NULL, CF = NULL,
                     minCases = NULL, fuzzyThreshold = NULL, sample = NULL,
                     earlyStopping = NULL) {
  
  args <- params(environment())
  mainargs <- names(args) %in% c("trials", "rules")
  params <- args[mainargs]
  params$control <- as.call(c(quote(C50::C5.0Control), args[!mainargs]))
  
  MLModel(
    name = "C50Model",
    packages = "C50",
    types = "factor",
    params = params,
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      mfit <- C50::C5.0(formula, data = data, weights = weights, ...)
      mfit$y <- response(formula, data)
      mfit
    },
    predict = function(object, newdata, ...) {
      predict(unMLModelFit(object), newdata = newdata, type = "prob")
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      C50::C5imp(object, ...)
    }
  )
}
