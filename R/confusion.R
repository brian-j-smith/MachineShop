#' Confusion Matrix
#' 
#' Calculate confusion matrices of predicted and observed responses.
#' 
#' @param x factor of observed responses or \code{Resamples} object of observed
#' and predicted responses.
#' @param y predicted responses.
#' @param cutoff threshold above which probabilities are classified as success
#' for binary responses.  If \code{NULL}, then responses are summed directly
#' over predicted class probabilities and will thus appear as decimal numbers
#' that can be interpreted as expected counts.
#' @param ... arguments passed to other methods.
#' 
#' @return
#' The return value is a \code{ConfusionMatrix} class object that inherits from
#' \code{table} if \code{x} is a factor or a \code{ConfusionResamples} object
#' that inherits from \code{list} if \code{x} is a \code{Resamples} object.
#'  
#' @seealso \code{\link{resample}}, \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' confusion(res)
#' 
confusion <- function(x, y = NULL, cutoff = 0.5, ...) {
  .confusion(x, y, cutoff = cutoff)
}


.confusion <- function(x, ...) {
  UseMethod(".confusion")
}


.confusion.factor <- function(x, y, cutoff, ...) {
  if (!is.null(cutoff)) y <- convert_response(x, y, cutoff = cutoff)
  ConfusionMatrix(.confusion_matrix(x, y))
}


.confusion.Resamples <- function(x, cutoff, ...) {
  if (!is.null(cutoff)) {
    x$Predicted <- convert_response(x$Observed, x$Predicted, cutoff = cutoff)
  }
  conf <- by(x, list(Model = x$Model), function(data) {
    ConfusionMatrix(.confusion_matrix(data$Observed, data$Predicted))
  }, simplify = FALSE)

  ConfusionResamples(structure(as(conf, "list"), names = names(conf)))

}


setGeneric(".confusion_matrix", function(observed, predicted, ...)
  standardGeneric(".confusion_matrix"))


setMethod(".confusion_matrix", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("confusion matrix requires a factor response variable")
  }
)


setMethod(".confusion_matrix", c("factor", "factor"),
  function(observed, predicted, ...) {
    table(Predicted = predicted, Observed = observed)
  }
)


setMethod(".confusion_matrix", c("factor", "matrix"),
  function(observed, predicted, ...) {
    df <- aggregate(predicted, list(observed), sum, na.rm = TRUE)
    df[, -1, drop = FALSE] %>%
      t %>%
      as.table %>%
      structure(dimnames = list(Predicted = df[[1]], Observed = df[[1]]))
  }
)


setMethod(".confusion_matrix", c("factor", "numeric"),
  function(observed, predicted, ...) {
    .confusion_matrix(observed, cbind(1 - predicted, predicted))
  }
)
