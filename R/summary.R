summary.Resamples <- function(object, ...) {
  sapply(object, function(metric, na.rm = TRUE, ...) {
    c("Mean" = mean(metric, na.rm = na.rm),
      "Median" = median(metric, na.rm = na.rm),
      "SD" = sd(metric, na.rm = na.rm),
      "Min" = min(metric, na.rm = na.rm),
      "Max" = max(metric, na.rm = na.rm),
      "NA" = mean(is.na(metric)))
  })
}
