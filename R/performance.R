#' Model Performance Metrics
#'
#' Compute measures of model performance.
#'
#' @name performance
#' @rdname performance
#'
#' @param x \link[=response]{observed responses}; or \link{confusion}, trained
#'   model \link{fit}, \link{resample}, or \link{rfe} result.
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
#' @param ... arguments passed from the \code{Resample} method to the response
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
#' pred <- predict(gbm_fit, newdata = veteran)
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


.performance <- function(x, y, weights, metrics, na.rm, ..., dots = list()) {
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
    metrics <- check_metrics(metrics, convert = TRUE)
    throw(check_assignment(metrics))
    do.call(metrics, args)
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
  metrics <- check_metrics(metrics, convert = TRUE)
  throw(check_assignment(metrics))
  do.call(metrics, args)
}


#' @rdname performance
#'
performance.MLModel <- function(x, ...) {
  if (!is_trained(x)) throw(Warning("No training results with performance."))
  ListOf(map(performance, x@steps))
}


#' @rdname performance
#'
performance.Resample <- function(x, ...) {
  perf_list <- by(x, x$Model, function(resamples) {
    Performance(performance(x@control, resamples, ...), control = x@control)
  }, simplify = FALSE)
  do.call(c, perf_list)
}


#' @rdname performance
#'
performance.TrainingStep <- function(x, ...) {
  x@performance
}


performance.MLControl <- function(x, resamples, ...) {
  perf_list <- by(resamples, resamples$Iteration, function(resample) {
    performance(resample$Observed, resample$Predicted, resample$Weight, ...)
  }, simplify = FALSE)
  do.call(rbind, perf_list)
}


performance.BootOptimismControl <- function(x, resamples, ...) {
  test_perf_list <- list()
  boot_perf_list <- list()
  resamples_split <- split(resamples, resamples$Iteration)
  for (name in names(resamples_split)) {
    resample <- resamples_split[[name]]
    test_perf_list[[name]] <- performance(
      resample$Observed, resample$Predicted, resample$Weight, ...
    )
    boot_perf_list[[name]] <- performance(
      resample$Boot.Observed, resample$Boot.Predicted, resample$Weight, ...
    )
  }
  test_perf <- do.call(rbind, test_perf_list)
  boot_perf <- do.call(rbind, boot_perf_list)
  train_perf <- performance(
    resample$Observed, resample$Train.Predicted, resample$Weight, ...
  )

  pessimism <- test_perf - boot_perf
  sweep(pessimism, 2, train_perf, "+")
}


performance.CVOptimismControl <- function(x, resamples, ...) {
  test_perf <- NextMethod()

  resamples_split <- split(resamples, ceiling(resamples$Iteration / x@folds))
  pred_names <- make_names_len(x@folds, "CV.Predicted.")
  perf_list <- map(function(resample) {
    f <- function(prop, pred) {
      prop * performance(resample$Observed, pred, resample$Weight, ...)
    }
    props <- proportions(table(resample$Iteration))
    preds <- resample[pred_names]
    Reduce("+", map(f, props, preds))
  }, resamples_split)
  cv_perf <- do.call(rbind, rep(perf_list, each = x@folds))
  train_perf <- performance(
    resamples_split[[1]]$Observed, resamples_split[[1]]$Train.Predicted,
    resamples_split[[1]]$Weight, ...
  )

  pessimism <- test_perf - cv_perf
  sweep(pessimism, 2, train_perf, "+")
}


Performance <- function(...) {
  object <- new("Performance", ...)
  ndim <- length(dimnames(object))
  if (ndim) {
    names(dimnames(object)) <- head(c("Iteration", "Metric", "Model"), ndim)
  }
  object
}
