summary.Resamples <- function(object,
                              stats = c("Mean" = mean,
                                        "Median" = median,
                                        "SD" = sd,
                                        "Min" = min,
                                        "Max" = max),
                              na.rm = TRUE, ...) {
  f <- function(x) {
    sapply(stats, function(stat) {
      c(stat(x, na.rm = na.rm))
    }) %>% c("NA" = mean(is.na(x)))
  }
  sapply(object, f)
}
