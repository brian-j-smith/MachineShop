#' Model Performance Summaries
#'
#' Summary statistics for resampled model performance metrics.
#'
#' @name summary
#' @rdname summary-methods
#'
#' @param object \link{confusion}, \link{lift}, trained model \link{fit},
#'   \link{performance}, \link[=curves]{performance curve}, or \link{resample}
#'   result.
#' @param stat function or character string naming a function to compute a
#'   summary statistic at each cutoff value of resampled metrics in
#'   \code{PerformanceCurve}, or \code{NULL} for resample-specific metrics.
#' @param stats function, function name, or vector of these with which to
#'   compute summary statistics.
#' @param na.rm logical indicating whether to exclude missing values.
#' @param ... arguments passed to other methods.
#'
#' @return An object of summmary statistics.
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Factor response example
#'
#' fo <- Species ~ .
#' control <- CVControl()
#'
#' gbm_res1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbm_res2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbm_res3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' summary(gbm_res3)
#'
#' res <- c(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
#' summary(res)
#' }
#'
NULL


#' @rdname summary-methods
#'
summary.ConfusionList <- function(object, ...) {
  ListOf(map(function(conf) summary(conf, ...), object))
}


#' @rdname summary-methods
#'
summary.ConfusionMatrix <- function(object, ...) {
  total <- sum(object)
  object <- object / total

  observed <- colSums(object)
  predicted <- rowSums(object)
  agreement <- diag(object)

  perf <- rbind(
    Observed = observed,
    Predicted = predicted,
    Agreement = agreement,
    Sensitivity = agreement / observed,
    Specificity = (1 - observed - predicted + agreement) / (1 - observed),
    PPV = agreement / predicted,
    NPV = (1 - observed - predicted + agreement) / (1 - predicted)
  )

  ConfusionSummary(perf,
                   total = total,
                   accuracy = sum(agreement),
                   majority = max(observed),
                   kappa2 = 1 - (1 - sum(agreement)) /
                     (1 - sum(observed * predicted)))
}


#' @rdname summary-methods
#'
summary.MLModel <- function(
  object, stats = MachineShop::settings("stats.Resample"), na.rm = TRUE, ...
) {
  if (!is_trained(object)) throw(Warning("No training results to summarize."))
  ListOf(map(summary, object@steps))
}


summary.MLModelFit <- function(object, ...) {
  summary(unMLModelFit(object))
}


#' @rdname summary-methods
#'
summary.Performance <- function(
  object, stats = MachineShop::settings("stats.Resample"), na.rm = TRUE, ...
) {
  stats <- check_stats(stats, convert = TRUE)
  throw(check_assignment(stats))

  f <- function(x) {
    prop_na <- mean(is.na(x))
    if (na.rm) x <- as.numeric(na.omit(x))
    c(stats(x), "NA" = prop_na)
  }

  margins <- 2
  perm <- c(2, 1)
  names <- c("Metric", "Statistic")
  if (ndim(object) == 3) {
    margins <- c(3, margins)
    perm <- c(perm, 3)
    names <- c("Model", "Statistic", "Metric")
  }
  object_summary <- aperm(apply(object, margins, f), perm = perm)
  names(dimnames(object_summary)) <- names
  TabularArray(object_summary)
}


#' @rdname summary-methods
#'
summary.PerformanceCurve <- function(
  object, stat = MachineShop::settings("stat.Curve"), ...
) {
  if (!(is.null(object$Iteration) || is.null(stat))) {

    stat <- check_stat(stat, convert = TRUE)
    throw(check_assignment(stat))

    object_class <- class(object)
    stat_na_omit <- function(x) stat(x[is.finite(x)])

    object_list <- by(object, object$Model, function(curves) {
      cutoffs <- unique(curves$Cutoff)
      curves_split <- split(curves, curves$Iteration)
      x_all <- y_all <- matrix(NA, length(cutoffs), length(curves_split))
      for (j in seq_along(curves_split)) {
        curve <- curves_split[[j]]
        x_all[, j] <- .curve_approx(curve$Cutoff, curve$x, cutoffs)
        y_all[, j] <- .curve_approx(curve$Cutoff, curve$y, cutoffs)
      }
      do.call(object_class, list(
        data.frame(Cutoff = cutoffs,
                   x = apply(x_all, 1, stat_na_omit),
                   y = apply(y_all, 1, stat_na_omit)),
        metrics = object@metrics
      ))
    })

    object <- do.call(c, object_list)
  }

  object
}


.curve_approx <- function(...) {
  values <- try(
    approx(..., method = "constant", rule = 2, f = 0.5),
    silent = TRUE
  )
  if (is(values, "try-error")) NA else values$y
}


#' @rdname summary-methods
#'
summary.Resample <- function(
  object, stats = MachineShop::settings("stats.Resample"), na.rm = TRUE, ...
) {
  summary(performance(object), stats = stats, na.rm = na.rm)
}


#' @rdname summary-methods
#'
summary.TrainingStep <- function(object, ...) {
  object@log
}
