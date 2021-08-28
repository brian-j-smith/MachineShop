#' Model Performance Metrics
#'
#' Compute measures of model performance.
#'
#' @name performance
#' @rdname performance
#'
#' @param x \link[=response]{observed responses}; or \link{confusion} or
#'   \link{resample} result containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param weights numeric vector of non-negative
#'   \link[=case_weights]{case weights} for the observed \code{x} responses
#'   [default: equal weights].
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.
#' @param cutoff numeric (0, 1) threshold above which binary factor
#'   probabilities are classified as events and below which survival
#'   probabilities are classified.
#' @param na.rm logical indicating whether to remove observed or predicted
#'   responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed from the \code{Resamples} method to the response
#'   type-specific methods or from the method for \code{ConfusionList} to
#'   \code{ConfusionMatrix}.  Elliptical arguments in the response
#'   type-specific methods are passed to \code{metrics} supplied as a single
#'   \code{\link[=metrics]{MLMetric}} function and are ignored otherwise.
#'
#' @seealso \code{\link{plot}}, \code{\link{summary}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' (perf <- performance(res))
#' summary(perf)
#' plot(perf)
#'
#' ## Survival response example
#' library(survival)
#'
#' gbm_fit <- fit(Surv(time, status) ~ ., data = veteran, model = GBMModel)
#'
#' obs <- response(gbm_fit, newdata = veteran)
#' pred <- predict(gbm_fit, newdata = veteran, type = "prob")
#' performance(obs, pred)
#' }
#'
performance <- function(x, ...) {
  UseMethod("performance")
}


#' @rdname performance
#'
performance.BinomialVariate <- function(
  x, y, weights = NULL, metrics = MachineShop::settings("metrics.numeric"),
  na.rm = TRUE, ...
) {
  .performance(x, y, weights, metrics, na.rm, dots = list(...))
}


#' @rdname performance
#'
performance.factor <- function(
  x, y, weights = NULL, metrics = MachineShop::settings("metrics.factor"),
  cutoff = MachineShop::settings("cutoff"), na.rm = TRUE, ...
) {
  .performance(x, y, weights, metrics, na.rm, cutoff = cutoff, dots = list(...))
}


#' @rdname performance
#'
performance.matrix <- function(
  x, y, weights = NULL, metrics = MachineShop::settings("metrics.matrix"),
  na.rm = TRUE, ...
) {
  .performance(x, y, weights, metrics, na.rm, dots = list(...))
}


#' @rdname performance
#'
performance.numeric <- function(
  x, y, weights = NULL, metrics = MachineShop::settings("metrics.numeric"),
  na.rm = TRUE, ...
) {
  .performance(x, y, weights, metrics, na.rm, dots = list(...))
}


#' @rdname performance
#'
performance.Surv <- function(
  x, y, weights = NULL, metrics = MachineShop::settings("metrics.Surv"),
  cutoff = MachineShop::settings("cutoff"), na.rm = TRUE, ...
) {
  .performance(x, y, weights, metrics, na.rm, cutoff = cutoff, dots = list(...))
}


.performance <- function(x, y, weights, metrics, na.rm, ..., dots = NULL) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y, weights = weights)
    x <- complete$x
    y <- complete$y
    weights <- complete$weights
  }
  if (length(x)) {
    args <- list(x, y, weights = weights, ...)
    metric <- if (is_one_element(metrics)) metrics[[1]] else metrics
    if (is(get0(metric), "MLMetric")) args <- c(args, dots)
    do.call(list_to_function(metrics, "metric"), args)
  } else {
    NA_real_
  }
}


#' @rdname performance
#'
performance.ConfusionList <- function(x, ...) {
  ListOf(map(function(conf) performance(conf, ...), x))
}


#' @rdname performance
#'
performance.ConfusionMatrix <- function(
  x, metrics = MachineShop::settings("metrics.ConfusionMatrix"), ...
) {
  args <- list(x)
  metric <- if (is_one_element(metrics)) metrics[[1]] else metrics
  if (is(get0(metric), "MLMetric")) args <- c(args, ...)
  do.call(list_to_function(metrics, "metric"), args)
}


#' @rdname performance
#'
performance.Resamples <- function(x, ...) {
  perf_list <- by(x, x$Model, function(resamples) {
    Performance(performance(x@control, resamples, ...))
  }, simplify = FALSE)
  do.call(c, perf_list)
}


performance.MLControl <- function(x, resamples, ...) {
  perf_list <- by(resamples, resamples$Resample, function(resample) {
    performance(resample$Observed, resample$Predicted, resample$Weight, ...)
  }, simplify = FALSE)
  do.call(rbind, perf_list)
}


performance.MLBootOptimismControl <- function(x, resamples, ...) {
  test_perf_list <- list()
  boot_perf_list <- list()
  resamples_split <- split(resamples, resamples$Resample)
  for (name in names(resamples_split)) {
    resample <- resamples_split[[name]]
    test_perf_list[[name]] <- performance(resample$Observed,
                                          resample$Predicted,
                                          resample$Weight, ...)
    boot_perf_list[[name]] <- performance(resample$Boot.Observed,
                                          resample$Boot.Predicted,
                                          resample$Weight, ...)
  }
  test_perf <- do.call(rbind, test_perf_list)
  boot_perf <- do.call(rbind, boot_perf_list)
  train_perf <- performance(resample$Observed, resample$Train.Predicted,
                            resample$Weight, ...)

  pessimism <- test_perf - boot_perf
  sweep(pessimism, 2, train_perf, "+")
}


performance.MLCVOptimismControl <- function(x, resamples, ...) {
  test_perf <- NextMethod()

  resamples_split <- split(resamples, ceiling(resamples$Resample / x@folds))
  vars <- paste0("CV.Predicted.", seq_len(x@folds))
  perf_list <- map(function(resample) {
    f <- function(p, pred) {
      p * performance(resample$Observed, pred, resample$Weight, ...)
    }
    Reduce("+", map(f, prop.table(table(resample$Resample)), resample[vars]))
  }, resamples_split)
  cv_perf <- do.call(rbind, rep(perf_list, each = x@folds))
  train_perf <- performance(resamples_split[[1]]$Observed,
                            resamples_split[[1]]$Train.Predicted,
                            resamples_split[[1]]$Weight, ...)

  pessimism <- test_perf - cv_perf
  sweep(pessimism, 2, train_perf, "+")
}


Performance <- function(...) {
  object <- new("Performance", ...)
  names <- c("Resample", "Metric")
  if (ndim(object) == 3) names <- c(names, "Model")
  names(dimnames(object)) <- names
  object
}
