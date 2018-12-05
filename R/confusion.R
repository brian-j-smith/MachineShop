#' Confusion Matrix
#' 
#' Calculate confusion matrices of predicted and observed responses.
#' 
#' @rdname confusion-methods
#' 
#' @param x observed responses or class containing observed and predicted
#' responses.
#' @param ... arguments passed to other methods.
#' 
confusion <- function(x, ...) {
  UseMethod("confusion")
}

#' @rdname confusion-methods
#' 
#' @details
#' \code{Resamples} responses are summed directly over predicted class
#' probabilities for the cross-classification and will thus appear as decimal
#' numbers that can be interpreted as expected counts.
#' 
#' @return \code{ConfusionResamples} class object that inherits from
#' \code{list}.
#'  
#' @seealso \code{\link{resample}}, \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' confusion(res)
#' 
confusion.Resamples <- function(x, ...) {
  conf <- by(x, list(Model = x$Model), function(data) {
    ConfusionMatrix(.confusion.Resamples(data$Observed, data$Predicted))
  }, simplify = FALSE)

  ConfusionResamples(structure(as(conf, "list"), names = names(conf)))
}


#' @rdname confusion-methods
#' 
#' @param y predicted responses.
#' @param cutoff threshold above which probabilities are classified as success
#' for binary responses.
#'
#' @return \code{ConfusionMatrix} class object that inherits from \code{table}.
#'  
confusion.factor <- function(x, y, cutoff = 0.5, ...) {
  ConfusionMatrix(.confusion(x, y, cutoff = cutoff))
}


setGeneric(".confusion", function(observed, predicted, ...)
  standardGeneric(".confusion"))


setMethod(".confusion", c("factor", "factor"),
  function(observed, predicted, ...) {
    table(Predicted = predicted, Observed = observed)
  }
)


setMethod(".confusion", c("factor", "matrix"),
  function(observed, predicted, ...) {
    predicted <- convert_response(observed, predicted)
    confusion(observed, predicted)
  }
)


setMethod(".confusion", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    predicted <- convert_response(observed, predicted, cutoff = cutoff)
    confusion(observed, predicted)
  }
)


setGeneric(".confusion.Resamples", function(observed, predicted, ...)
  standardGeneric(".confusion.Resamples"))


setMethod(".confusion.Resamples", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("confusion matrix requires a factor response variable")
  }
)


setMethod(".confusion.Resamples", c("factor", "matrix"),
  function(observed, predicted, ...) {
    df <- aggregate(predicted, list(observed), sum, na.rm = TRUE)
    conf_tbl <- as.table(as.matrix(df[, -1, drop = FALSE]))
    dimnames(conf_tbl) <- list(Observed = df[[1]], Predicted = df[[1]])
    t(conf_tbl)
  }
)


setMethod(".confusion.Resamples", c("factor", "numeric"),
  function(observed, predicted, ...) {
    predicted <- cbind(1 - predicted, predicted)
    colnames(predicted) <- levels(observed)
    .confusion.Resamples(observed, predicted)
  }
)
