#' Performance Metrics
#'
#' Compute measures of agreement between observed and predicted responses.
#'
#' @name metrics
#'
#' @param observed \link[=response]{observed responses}; or
#'   \link{confusion}, \link[=curves]{performance curve}, or \link{resample}
#'   result containing observed and predicted responses.
#' @param predicted \link[=predict]{predicted responses} if not contained in
#'   \code{observed}.
#' @param weights numeric vector of non-negative
#'   \link[=case_weights]{case weights} for the observed responses [default:
#'   equal weights].
#' @param beta relative importance of recall to precision in the calculation of
#'   \code{f_score} [default: F1 score].
#' @param cutoff numeric (0, 1) threshold above which binary factor
#'   probabilities are classified as events and below which survival
#'   probabilities are classified.
#' @param distr character string specifying a distribution with which to
#'   estimate the observed survival mean in the total sum of square component of
#'   \code{r2}.  Possible values are \code{"empirical"} for the Kaplan-Meier
#'   estimator, \code{"exponential"}, \code{"extreme"}, \code{"gaussian"},
#'   \code{"loggaussian"}, \code{"logistic"}, \code{"loglogistic"},
#'   \code{"lognormal"}, \code{"rayleigh"}, \code{"t"}, or \code{"weibull"}.
#'   Defaults to the distribution that was used in predicting mean survival
#'   times.
#' @param f function to calculate a desired sensitivity-specificity tradeoff.
#' @param metrics vector of two metric functions or function names that define a
#'   curve under which to calculate area [default: ROC metrics].
#' @param power power to which positional distances of off-diagonals from the
#'   main diagonal in confusion matrices are raised to calculate
#'   \code{weighted_kappa2}.
#' @param stat function or character string naming a function to compute a
#'   summary statistic at each cutoff value of resampled metrics in performance
#'   curves, or \code{NULL} for resample-specific metrics.
#' @param ... arguments passed to or from other methods.
#'
#' @seealso \code{\link{metricinfo}}, \code{\link{performance}}
#'
NULL


#################### Metric Method Functions ####################


setMetric_auc <- function(f, metrics) {
  definition <- function(observed, predicted, ...) {
    auc(observed, predicted, metrics = metrics, ...)
  }
  setMetricGeneric(f)
  setMetricMethod(f, c("factor", "factor"))
  setMetricMethod(f, c("factor", "numeric"), definition)
  setMetricMethod_Resamples(f)
  setMetricMethod(f, c("Surv", "SurvProbs"), definition)
}


setMetric_BinaryConfusionMatrix <- function(f, definition) {
  setMetricGeneric(f)
  setMetricMethod(f, c("BinaryConfusionMatrix", "NULL"), definition)
  setMetricMethod_factor_factor(f)
  setMetricMethod_factor_numeric(f)
  setMetricMethod_Resamples(f)
  setMetricMethod_Surv_SurvEvents(f)
  setMetricMethod_Surv_SurvProbs(f)
}


setMetric_ConfusionMatrix <- function(f, definition) {
  setMetricGeneric(f)
  setMetricMethod(f, c("ConfusionMatrix", "NULL"), definition)
  setMetricMethod_factor_factor(f)
  setMetricMethod_factor_matrix(f)
  setMetricMethod_factor_numeric(f)
  setMetricMethod_Resamples(f)
  setMetricMethod_Surv_SurvEvents(f)
  setMetricMethod_Surv_SurvProbs(f)
}


setMetric_OrderedConfusionMatrix <- function(f, definition) {
  setMetricGeneric(f)
  setMetricMethod(f, c("OrderedConfusionMatrix", "NULL"), definition)
  definition_ordered <- function(observed, predicted, weights, ...) {
    get(f)(confusion(observed, predicted, weights), ...)
  }
  setMetricMethod(f, c("ordered", "ordered"), definition_ordered)
  setMetricMethod(f, c("ordered", "matrix"), definition_ordered)
  setMetricMethod_Resamples(f)
}


setMetric_numeric <- function(f, definition) {
  setMetricGeneric(f)
  setMetricMethod(f, c("numeric", "numeric"), definition)
  setMetricMethod_BinomialMatrix_numeric(f)
  setMetricMethod_matrix_matrix(f)
  setMetricMethod_Resamples(f)
  setMetricMethod_Surv_numeric(f)
}


setMetricGeneric <- function(f) {

  eval(substitute(
    setGeneric(name, function(observed, predicted, ...) standardGeneric(name)),
    list(name = metric_method_name(f))
  ))

  setMetricMethod(f, c("ANY", "ANY"))

}


setMetricMethod <- function(
  f, signature, definition = function(observed, predicted, ...) numeric()
) {
  setMethod(metric_method_name(f), signature, definition)
}


setMetricMethod_BinomialMatrix_numeric <- function(f) {
  setMetricMethod(f, c("BinomialVariate", "numeric"),
    function(observed, predicted, ...) {
      get(f)(as.numeric(observed), predicted, ...)
    }
  )
}


setMetricMethod_factor_factor <- function(f) {
  setMetricMethod(f, c("factor", "factor"),
    function(observed, predicted, weights, ...) {
      get(f)(confusion(observed, predicted, weights), ...)
    }
  )
}


setMetricMethod_factor_matrix <- function(f) {
  setMetricMethod(f, c("factor", "matrix"),
    function(observed, predicted, weights, ...) {
      get(f)(confusion(observed, predicted, weights), ...)
    }
  )
}


setMetricMethod_factor_numeric <- function(f) {
  setMetricMethod(f, c("factor", "numeric"),
    function(observed, predicted, weights, cutoff, ...) {
      get(f)(confusion(observed, predicted, weights, cutoff = cutoff), ...)
    }
  )
}


setMetricMethod_matrix_matrix <- function(f) {
  setMetricMethod(f, c("matrix", "matrix"),
    function(observed, predicted, ...) {
      metric_matrix(get(f), observed, predicted, ...)
    }
  )
}


setMetricMethod_Resamples <- function(f) {
  setMetricMethod(f, c("Resamples", "NULL"),
    function(observed, predicted, weights, ...) {
      performance(observed, metrics = get(f), ...)
    }
  )
}


setMetricMethod_Surv_numeric <- function(f) {
  setMetricMethod(f, c("Surv", "numeric"),
    function(observed, predicted, ...) {
      metric_SurvTimes(get(f), observed, predicted, ...)
    }
  )
}


setMetricMethod_Surv_SurvEvents <- function(f) {
  setMetricMethod(f, c("Surv", "SurvEvents"),
    function(observed, predicted, ...) {
      metric_SurvMatrix(get(f), observed, predicted, ...)
    }
  )
}


setMetricMethod_Surv_SurvProbs <- function(f) {
  setMetricMethod(f, c("Surv", "SurvProbs"),
    function(observed, predicted, ...) {
      metric_SurvMatrix(get(f), observed, predicted, ...)
    }
  )
}


#################### Utility Functions ####################


call_metric_method <- function(f, envir) {
  do.call(metric_method_name(f), as.list(envir))
}


metric_method_name <- function(f) {
  paste0(".", f)
}


metric_matrix <- function(FUN, observed, predicted, ...) {
  mean(map_num(function(i) {
    FUN(observed[, i], predicted[, i], ...)
  }, seq_len(ncol(observed))))
}


metric_SurvMatrix <- function(
  FUN, observed, predicted, weights, cutoff = numeric(), ...
) {
  conf_list <- confusion(observed, predicted, weights, cutoff = cutoff)
  x <- map_num(function(conf) FUN(conf, ...), conf_list)
  times <- predicted@times
  if (length(times) > 1) c("mean" = survmetric_mean(x, times), x) else x[[1]]
}


metric_SurvTimes <- function(FUN, observed, predicted, weights = NULL, ...) {
  events <- observed[, "status"] == 1
  FUN(time(observed[events]), predicted[events], weights = weights[events], ...)
}


weighted_mean <- function(x, weights = NULL) {
  if (is.null(weights)) {
    sum(x) / length(x)
  } else {
    weights <- check_weights(weights, x)
    throw(check_assignment(weights))
    sum(weights * x) / sum(weights)
  }
}


weighted_sd <- function(x, weights = NULL) {
  n <- length(x)
  sqrt(mse(x, weighted_mean(x, weights), weights) * n / (n - 1))
}
