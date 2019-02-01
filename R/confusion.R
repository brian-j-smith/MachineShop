#' Confusion Matrix
#' 
#' Calculate confusion matrices of predicted and observed responses.
#' 
#' @rdname confusion
#' 
#' @param x factor of observed responses or \code{Resamples} object of observed
#' and predicted responses.
#' @param y predicted responses.
#' @param cutoff threshold above which binary factor probabilities are
#' classified as events and below which survival probabilities are classified.
#' If \code{NULL}, then binary responses are summed directly over predicted
#' class probabilities, whereas a default cutoff of 0.5 is used for
#' survival probabilities.  Class probability summations and survival will
#' appear as decimal numbers that can be interpreted as expected counts.
#' @param times numeric vector of follow-up times if \code{y} contains predicted
#' survival probabilities.
#' 
#' @return
#' The return value is a \code{ConfusionMatrix} class object that inherits from
#' \code{table} if \code{x} and \code{y} responses are specified or a
#' \code{ConfusionResamples} object that inherits from \code{list} if \code{x}
#' is a \code{Resamples} object.
#'  
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' confusion(res)
#' 
confusion <- function(x, y = NULL, cutoff = 0.5, times = numeric(), ...) {
  .confusion(x, y, cutoff = cutoff, times = times)
}


.confusion <- function(x, ...) {
  UseMethod(".confusion")
}


.confusion.default <- function(x, y, cutoff, ...) {
  if (!is.null(cutoff)) y <- convert_response(x, y, cutoff = cutoff)
  ConfusionMatrix(.confusion_matrix(x, y))
}


.confusion.Resamples <- function(x, cutoff, ...) {
  conf_list <- by(x, list(Model = x$Model), function(data) {
   confusion(data$Observed, data$Predicted, cutoff = cutoff,
             times = x@control@times)
  }, simplify = FALSE)
  if (all(mapply(is, conf_list, "Confusion"))) {
    conf_list <- unlist(conf_list, recursive = FALSE)
  }
  do.call(Confusion, conf_list)
}


.confusion.Surv <- function(x, y, cutoff, times, ...) {
  if (is.null(cutoff)) cutoff <- 0.5
  y <- convert_response(x, y, cutoff = cutoff)
  do.call(Confusion, .confusion_matrix(x, y, times = times))
}


setGeneric(".confusion_matrix", function(observed, predicted, ...)
  standardGeneric(".confusion_matrix"))


setMethod(".confusion_matrix", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("confusion requires a predicted factor or survival probabilities")
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


setMethod(".confusion_matrix", c("Surv", "matrix"),
  function(observed, predicted, times, ...) {
    if (length(times) != ncol(predicted)) {
      stop("unequal number of survival times and predictions")
    }
    
    surv_all <- predict(survfit(observed ~ 1, se.fit = FALSE), times)

    structure(
      lapply(1:ncol(predicted), function(i) {
        surv_pos <- 1
        positives <- predicted[, i] == 1
        p <- mean(positives)
        if (p > 0) {
          obs <- observed[positives]
          valid_events <- obs[, "status"] == 1 & obs[, "time"] <= times[i]
          event_times <- sort(unique(obs[valid_events, "time"]))
          for (event_time in event_times) {
            d <- sum(obs[, "time"] == event_time & obs[, "status"] == 1)
            n <- sum(obs[, "time"] >= event_time)
            surv_pos <- surv_pos * (1 - d / n)
          }
        }

        conf_tbl <- table(Predicted = 0:1, Observed = 0:1)
        conf_tbl[1, 1] <- surv_all[i] - surv_pos * p
        conf_tbl[1, 2] <- (1 - p) - conf_tbl[1, 1]
        conf_tbl[2, 2] <- (1 - surv_pos) * p
        conf_tbl[2, 1] <- p - conf_tbl[2, 2]
        
        ConfusionMatrix(length(observed) * conf_tbl)
      }),
      names = paste0("time", seq_along(times))
    )
  }
)
