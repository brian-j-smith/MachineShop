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
#' \item{\link[=tune]{Automatic Tuning} Grid Parameters:}{
#'   \code{trials}, \code{rules}, \code{winnow}
#' }
#' }
#' 
#' Latter arguments are passed to \code{\link[C50]{C5.0Control}}.
#' Further model details can be found in the source link below.
#' 
#' In calls to \code{\link{varimp}} for \code{C50Model}, argument \code{metric}
#' may be spedified as \code{"usage"} (default) for the percentage of training
#' set samples that fall into all terminal nodes after the split of each
#' predictor or as \code{"splits"} for the percentage of splits associated with
#' each predictor.  Variable importance is automatically scaled to range from 0
#' to 100.  To obtain unscaled importance values, set \code{scale = FALSE}.  See
#' example below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[C50]{C5.0}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' modelfit <- fit(Species ~ ., data = iris, model = C50Model())
#' varimp(modelfit, metric = "splits", scale = FALSE)
#'
C50Model <- function(trials = 1, rules = FALSE, subset = TRUE, bands = 0,
                     winnow = FALSE, noGlobalPruning = FALSE, CF = 0.25,
                     minCases = 2, fuzzyThreshold = FALSE, sample = 0,
                     earlyStopping = TRUE) {
  
  args <- params(environment())
  is_main <- names(args) %in% c("trials", "rules")
  params <- args[is_main]
  params$control <- as.call(c(.(C50::C5.0Control), args[!is_main]))
  
  MLModel(
    name = "C50Model",
    label = "C5.0 Classification",
    packages = "C50",
    response_types = "factor",
    params = params,
    grid = function(x, length, ...) {
      list(
        trials = c(1, round(seq_range(0, 10, c(2, 100), length))),
        rules = c(FALSE, TRUE),
        winnow = c(FALSE, TRUE)
      )
    },
    design = "terms",
    fit = function(formula, data, weights, ...) {
      eval_fit(data,
               formula = C50::C5.0(formula, data = as.data.frame(data),
                                   weights = weights, ...),
               matrix = C50::C5.0(x, y, weights = weights, ...))
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "prob")
    },
    varimp = function(object, metric = c("usage", "splits"), ...) {
      C50::C5imp(object, metric = match.arg(metric))
    }
  )
  
}
