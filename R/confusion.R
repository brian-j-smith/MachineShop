#' Confusion Matrix
#' 
#' Calculate confusion matrices of cross-classified predicted and observed
#' responses.
#' 
#' @param x \code{Resamples} object.
#' @param ... arguments passed to other methods.
#' 
#' @details
#' Responses are summed directly over predicted class probabilities for the
#' cross-classification and will thus appear as decimal numbers that can be
#' interpreted as expected counts.
#' 
#' @return \code{ConfusionResamples} class object that inherits from
#' \code{list}.
#'  
#' @seealso \code{\link{resample}}
#' 
#' @examples
#' perf <- resample(Species ~ ., data = iris, model = GBMModel)
#' confusion(perf)
#' 
confusion <- function(x, ...) {
  stopifnot(is(x, "Resamples"))
  
  conf <- by(x, list(Model = x$Model), function(data) {
    .confusion(data$Observed, data$Predicted)
  }, simplify = FALSE)
  
  structure(as(conf, "list"), names = names(conf),
            class = c("ConfusionResamples", "listof"))
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
    t(conf_tbl)
  }
)


setMethod(".confusion", c("factor", "numeric"),
  function(observed, predicted, ...) {
    predicted <- cbind(1 - predicted, predicted)
    colnames(predicted) <- levels(observed)
    .confusion(observed, predicted)
  }
)
