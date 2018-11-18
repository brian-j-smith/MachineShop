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
#' @param bands integer between 2 and 1000 specifying a number of bands into 
#' which to group rules ordered by their affect on the error rate.
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
#' Further model details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[C50]{C5.0}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = C50Model())
#'
C50Model <- function(trials = 1, rules = FALSE, subset = TRUE, bands = 0,
                     winnow = FALSE, noGlobalPruning = FALSE, CF = 0.25,
                     minCases = 2, fuzzyThreshold = FALSE, sample = 0,
                     earlyStopping = TRUE) {
  
  args <- params(environment())
  mainargs <- names(args) %in% c("trials", "rules")
  params <- args[mainargs]
  params$control <- as.call(c(.(C50::C5.0Control), args[!mainargs]))
  
  MLModel(
    name = "C50Model",
    packages = "C50",
    types = "factor",
    params = params,
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      C50::C5.0(formula, data = data, weights = weights, ...)
    },
    predict = function(object, newdata, ...) {
      predict(unMLModelFit(object), newdata = newdata, type = "prob")
    },
    varimp = function(object, ...) {
      C50::C5imp(object, ...)
    }
  )
}
