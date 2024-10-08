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
#'   \item{Response types:}{\code{factor}, \code{numeric}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{mtry}, \code{min.node.size}*, \code{splitrule}*
#'   }
#' }
#' * excluded from grids by default
#'
#' Default argument values and further model details can be found in the source
#' See Also link below.
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
  num.trees = 500, mtry = integer(),
  importance = c("impurity", "impurity_corrected", "permutation"),
  min.node.size = integer(), replace = TRUE,
  sample.fraction = if (replace) 1 else 0.632, splitrule = character(),
  num.random.splits = 1, alpha = 0.5, minprop = 0.1,
  split.select.weights = numeric(), always.split.variables = character(),
  respect.unordered.factors = character(), scale.permutation.importance = FALSE,
  verbose = FALSE
) {

  importance <- match.arg(importance)

  MLModel(

    name = "RangerModel",
    label = "Fast Random Forests",
    packages = "ranger",
    response_types = c("factor", "numeric", "Surv"),
    weights = TRUE,
    predictor_encoding = "model.frame",
    na.rm = TRUE,
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("mtry", "min.node.size", "splitrule"),
      get_values = c(
        function(n, data, ...) seq_nvars(data, RangerModel, n),
        function(n, data, ...) {
          round_int(seq(1, min(20, nrow(data)), length = n))
        },
        function(n, data, ...) {
          methods <- switch_class(response(data),
            "factor" = c("gini", "extratrees"),
            "numeric" = c("variance", "extratrees", "maxstat"),
            "Surv" = c("logrank", "extratrees", "C", "maxstat")
          )
          head(methods, n)
        }
      ),
      default = c(TRUE, FALSE, FALSE)
    ),

    fit = function(formula, data, weights, ...) {
      ranger::ranger(
        formula, data = as.data.frame(formula, data = data),
        case.weights = weights, probability = is(response(data), "factor"), ...
      )
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      pred <- predict(object, data = newdata)
      if (!is.null(object$survival)) {
        predict(Surv(pred$unique.death.times), pred$survival, ...)
      } else {
        pred$predictions
      }
    },

    varimp = function(object, ...) {
      structure(ranger::importance(object), metric = object$splitrule)
    }

  )

}

MLModelFunction(RangerModel) <- NULL
