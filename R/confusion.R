#' Confusion Matrix
#' 
#' Calculate confusion matrices summarizing the proportions of cross-classified
#' observed and predicted responses
#' 
#' @param x Resamples object.
#' @param ... arguments passed to other methods.
#' 
#' @return \code{by} class object that inherits from \code{list}.
#'  
#' @seealso \code{\link{resample}}
#' 
#' @examples
#' perf <- resample(Species ~ ., data = iris, model = GBMModel)
#' confusion(perf)
#' 
confusion <- function(x, ...) {
  stopifnot(is(x, "Resamples"))
  
  by(response(x), list(Model = response(x)$Model), function(data) {
    .confusion(data$Observed, data$Predicted)
  }, simplify = FALSE)
}


setGeneric(".confusion", function(observed, predicted, ...)
  standardGeneric(".confusion"))


setMethod(".confusion", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("confusion matrix requires a factor response variable")
  }
)


setMethod(".confusion", c("factor", "matrix"),
  function(observed, predicted, ...) {
    df <- aggregate(predicted, list(observed), sum, na.rm = TRUE)
    conf_tbl <- as.table(as.matrix(df[, -1, drop = FALSE]))
    dimnames(conf_tbl) <- list(Observed = df[[1]], Predicted = names(df)[-1])
    conf_tbl / sum(conf_tbl)
  }
)


setMethod(".confusion", c("factor", "numeric"),
  function(observed, predicted, ...) {
    predicted <- cbind(1 - predicted, predicted)
    colnames(predicted) <- levels(observed)
    .confusion(observed, predicted)
  }
)
