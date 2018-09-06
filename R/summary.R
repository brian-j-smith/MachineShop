#' Model Performance Summary
#' 
#' Summary statistics for resampled model performance metrics.
#' 
#' @name summary
#' 
#' @param object object of resampled metrics as returned by
#' \code{\link{resample}}.
#' @param stats list of named functions to include in the calculation of summary
#' statistics.  Supplied functions should contain a \code{na.rm} argument in
#' their definitions.
#' @param na.rm logical indicating whether to exclude missing values.
#' @param ... arguments to be passed to other methods.
#' 
#' @seealso \code{\link{resample}}
#' 
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
  apply(object, 2, f)
}


summary.MLControl <- function(object, observed, predicted, ...) {
  do.call(object@summary, c(list(observed = observed, predicted = predicted),
                            as(object, "list")))
}
