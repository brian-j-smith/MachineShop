#' Model Performance Summary
#' 
#' Summary statistics for resampled model performance metrics.
#' 
#' @name summary
#' @rdname summary-methods
#' 
#' @param object object to summarize.
#' @param stats function or list of named functions to include in the
#' calculation of summary statistics.
#' @param na.rm logical indicating whether to exclude missing values.
#' @param ... arguments passed to other methods.
#' 
#' @return array with summmary statistics in the second dimension, metrics in
#' the first for single models, and models and metrics in the first and third,
#' respectively, for multiple models.
#' 
#' @seealso \code{\link{diff}}, \code{\link{resample}}, \code{\link{Resamples}},
#' \code{\link{tune}}
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- factor(Species) ~ .
#' control <- CVControl()
#' 
#' gbmperf1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbmperf2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbmperf3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' summary(gbmperf3)
#' 
#' perf <- Resamples(GBM1 = gbmperf1, GBM2 = gbmperf2, GBM3 = gbmperf3)
#' summary(perf)
#' 
summary.MLModelTune <- function(object,
                                stats = c("Mean" = mean,
                                          "Median" = median,
                                          "SD" = sd,
                                          "Min" = min,
                                          "Max" = max),
                                na.rm = TRUE, ...) {
  summary(object@resamples, stats = stats, na.rm = na.rm, ...)
}


#' @rdname summary-methods
#' 
summary.Resamples <- function(object,
                              stats = c("Mean" = mean,
                                        "Median" = median,
                                        "SD" = sd,
                                        "Min" = min,
                                        "Max" = max),
                              na.rm = TRUE, ...) {
  if (is.list(stats)) {
    stats <- eval(bquote(
      function(x) sapply(.(stats), function(stat) stat(x))
    ))
  }
  
  f <- function(x) {
    prop_na <- mean(is.na(x))
    if (na.rm) x <- na.omit(x)
    c(stats(x), "NA" = prop_na)
  }
  
  margins <- 2
  perm <- c(2, 1)
  if (length(dim(object)) > 2) {
    margins <- c(3, margins)
    perm <- c(perm, 3)
  }
  aperm(apply(object, margins, f), perm = perm)
}


summary.MLControl <- function(object, observed, predicted, ...) {
  if (object@na.rm) {
    df <- data.frame(
      observed = I(observed),
      predicted = I(predicted)
    ) %>% na.omit
    observed <- unAsIs(df$observed)
    predicted <- unAsIs(df$predicted)
  }
  do.call(object@summary, c(list(observed = observed, predicted = predicted),
                            as(object, "list")))
}


summary.MLModelFit <- function(object, ...) {
  summary(unMLModelFit(object))
}
