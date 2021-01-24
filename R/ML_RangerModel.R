#' Fast Random Forest Model
#'
#' Fast implementation of random forests or recursive partitioning.
#'
#' @param num.trees number of trees.
#' @param mtry number of variables to possibly split at in each node.
#' @param importance variable importance mode.
#' @param min.node.size minimum node size.
#' @param replace logical indicating whether to sample with replacement.
#' @param sample.fraction fraction of observations to sample.
#' @param splitrule splitting rule.
#' @param num.random.splits number of random splits to consider for each
#'   candidate splitting variable in the \code{"extratrees"} rule.
#' @param alpha significance threshold to allow splitting in the
#'   \code{"maxstat"} rule.
#' @param minprop lower quantile of covariate distribution to be considered for
#'   splitting in the \code{"maxstat"} rule.
#' @param split.select.weights numeric vector with weights between 0 and 1,
#'   representing the probability to select variables for splitting.
#' @param always.split.variables character vector with variable names to be
#'   always selected in addition to the \code{mtry} variables tried for
#'   splitting.
#' @param respect.unordered.factors handling of unordered factor covariates.
#' @param scale.permutation.importance scale permutation importance by
#'   standard error.
#' @param verbose show computation status and estimated runtime.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{mtry}, \code{min.node.size}*, \code{splitrule}*
#'   }
#' }
#' * included only in randomly sampled grid points
#'
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[ranger]{ranger}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package ranger to run
#'
#' fit(Species ~ ., data = iris, model = RangerModel)
#' }
#'
RangerModel <- function(
  num.trees = 500, mtry = NULL,
  importance = c("impurity", "impurity_corrected", "permutation"),
  min.node.size = NULL, replace = TRUE,
  sample.fraction = ifelse(replace, 1, 0.632), splitrule = NULL,
  num.random.splits = 1, alpha = 0.5, minprop = 0.1,
  split.select.weights = NULL, always.split.variables = NULL,
  respect.unordered.factors = NULL, scale.permutation.importance = FALSE,
  verbose = FALSE
) {

  importance <- match.arg(importance)

  MLModel(
    name = "RangerModel",
    label = "Fast Random Forests",
    packages = "ranger",
    response_types = c("factor", "numeric", "Surv"),
    predictor_encoding = "terms",
    params = params(environment()),
    grid = function(x, length, random, ...) {
      params <- list(
        mtry = seq_nvars(x, RangerModel, length)
      )
      if (random) {
        params$min.node.size <- round(seq(1, min(20, nrow(x)), length = length))
        splitrule <- if (is.factor(response(x))) {
          c("gini", "extratrees")
        } else {
          c("variance", "extratrees", "maxstat")
        }
        params$splitrule <- head(sample(splitrule), length)
      }
      params
    },
    fit = function(formula, data, weights, ...) {
      ranger::ranger(formula, data = as.data.frame(data),
                     case.weights = weights,
                     probability = is(response(data), "factor"), ...)
    },
    predict = function(object, newdata, times, ...) {
      newdata <- as.data.frame(newdata)
      pred <- predict(object, data = newdata)
      if (!is.null(object$survival)) {
        predict(Surv(pred$unique.death.times), pred$survival, times, ...)
      } else {
        pred$predictions
      }
    },
    varimp = function(object, ...) {
      ranger::importance(object)
    }
  )

}

MLModelFunction(RangerModel) <- NULL
