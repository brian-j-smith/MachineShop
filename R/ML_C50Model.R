#' C5.0 Decision Trees and Rule-Based Model
#'
#' Fit classification tree models or rule-based models using Quinlan's C5.0
#' algorithm.
#'
#' @param trials integer number of boosting iterations.
#' @param rules logical indicating whether to decompose the tree into a
#'   rule-based model.
#' @param subset logical indicating whether the model should evaluate groups of
#'   discrete predictors for splits.
#' @param bands integer between 2 and 1000 specifying a number of bands into
#'   which to group rules ordered by their affect on the error rate.
#' @param winnow logical indicating use of predictor winnowing (i.e. feature
#'   selection).
#' @param noGlobalPruning logical indicating a final, global pruning step to
#'   simplify the tree.
#' @param CF number in (0, 1) for the confidence factor.
#' @param minCases integer for the smallest number of samples that must be put
#'   in at least two of the splits.
#' @param fuzzyThreshold logical indicating whether to evaluate possible
#'   advanced splits of the data.
#' @param sample value between (0, 0.999) that specifies the random proportion
#'   of data to use in training the model.
#' @param earlyStopping logical indicating whether the internal method for
#'   stopping boosting should be used.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{trials}, \code{rules}, \code{winnow}
#'   }
#' }
#'
#' Latter arguments are passed to \code{\link[C50]{C5.0Control}}.
#' Further model details can be found in the source link below.
#'
#' In calls to \code{\link{varimp}} for \code{C50Model}, argument \code{type}
#' may be specified as \code{"usage"} (default) for the percentage of training
#' set samples that fall into all terminal nodes after the split of each
#' predictor or as \code{"splits"} for the percentage of splits associated with
#' each predictor.  Variable importance is automatically scaled to range from 0
#' to 100.  To obtain unscaled importance values, set \code{scale = FALSE}.  See
#' example below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[C50]{C5.0}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package C50 to run
#'
#' model_fit <- fit(Species ~ ., data = iris, model = C50Model)
#' varimp(model_fit, method = "model", type = "splits", scale = FALSE)
#' }
#'
C50Model <- function(
  trials = 1, rules = FALSE, subset = TRUE, bands = 0, winnow = FALSE,
  noGlobalPruning = FALSE, CF = 0.25, minCases = 2, fuzzyThreshold = FALSE,
  sample = 0, earlyStopping = TRUE
) {

  MLModel(

    name = "C50Model",
    label = "C5.0 Classification",
    packages = "C50",
    response_types = "factor",
    weights = TRUE,
    predictor_encoding = "model.frame",
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("trials", "rules", "winnow"),
      get_values = c(
        function(n, ...) c(1, round(seq_range(0, 10, c(2, 100), n))),
        function(...) c(FALSE, TRUE),
        function(...) c(FALSE, TRUE)
      )
    ),

    fit = function(formula, data, weights, trials, rules, ...) {
      control <- C50::C5.0Control(...)
      eval_fit(
        data,
        formula = C50::C5.0(
          formula, data = data, weights = weights, trials = trials,
          rules = rules, control = control
        ),
        matrix = C50::C5.0(
          x, y, weights = weights, trials = trials, rules = rules,
          control = control
        )
      )
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "prob")
    },

    varimp = function(object, type = c("usage", "splits"), ...) {
      C50::C5imp(object, metric = match.arg(type))
    }

  )

}

MLModelFunction(C50Model) <- NULL
