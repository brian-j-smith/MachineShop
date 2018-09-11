#' Model Performance Summary
#' 
#' Summary statistics for resampled model performance metrics.
#' 
#' @name summary
#' @rdname summary-methods
#' 
#' @param object object to summarize.
#' @param stats list of named functions to include in the calculation of summary
#' statistics.  Supplied functions should contain a \code{na.rm} argument in
#' their definitions.
#' @param na.rm logical indicating whether to exclude missing values.
#' @param ... arguments passed to other methods.
#' 
#' @seealso \code{\link{resample}}, \code{\link{Resamples}}, \code{\link{tune}}
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
  f <- function(x) {
    sapply(stats, function(stat) {
      c(stat(x, na.rm = na.rm))
    }) %>% c("NA" = mean(is.na(x)))
  }
  margins <- if(length(dim(object)) > 2) c(3, 2) else 2
  apply(object, margins, f)
}


summary.MLControl <- function(object, observed, predicted, ...) {
  do.call(object@summary, c(list(observed = observed, predicted = predicted),
                            as(object, "list")))
}
