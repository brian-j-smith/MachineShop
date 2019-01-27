#' Model Lift
#' 
#' Calculate lift estimates from observed and predicted responses.
#' 
#' @rdname lift
#' 
#' @param x observed responses or \code{Resamples} object of observed and
#' predicted responses.
#' @param y predicted responses.
#' @param times numeric vector of follow-up times if \code{y} contains predicted
#' survival events.
#' 
#' @return \code{Lift} class object that inherits from \code{data.frame}.
#'  
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{plot}}
#' 
#' @examples
#' library(MASS)
#' 
#' res <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#' (lf <- lift(res))
#' plot(lf)
#' 
lift <- function(x, y = NULL, times = numeric(), ...) {
  .lift(x, y, times = times)
}


.lift <- function(x, ...) {
  UseMethod(".lift")
}


.lift.default <- function(x, y, times, ...) {
  Lift(.lift_default(x, y, times = times))
}


.lift.Resamples <- function(x, ...) {
  times <- x@control@surv_times
  lf_list <- by(x, x$Model, function(data) {
    .lift_default(data$Observed, data$Predicted, times = times)
  }, simplify = FALSE)
  if (all(mapply(is, lf_list, "list"))) {
    lf_list <- unlist(lf_list, recursive = FALSE)
  }
  do.call(Lift, lf_list)
}


.lift.Surv <- function(x, y, times, ...) {
  do.call(Lift, .lift_default(x, y, times = times))
}


setGeneric(".lift_default", function(observed, predicted, ...)
  standardGeneric(".lift_default"))


setMethod(".lift_default", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("lift requires a predicted binary factor or survival probabilities")
  }
)


setMethod(".lift_default", c("factor", "numeric"),
  function(observed, predicted, ...) {
    df <- data.frame(
      observed = as.integer(observed == levels(observed)[2]),
      predicted = predicted
    ) %>% na.omit
    observed <- rev(rowsum(df$observed, df$predicted))
    100 * data.frame(
      Found = c(0, cumsum(observed) / sum(observed)),
      Tested = c(0, seq(observed) / length(observed))
    )
  }
)


setMethod(".lift_default", c("Surv", "matrix"),
  function(observed, predicted, times, ...) {
    structure(
      lapply(1:ncol(predicted), function(i) {
        pred <- predicted[, i, drop = FALSE]
        cutoffs <- sort(unique(pred))
        time <- times[i]
        
        found <- numeric()
        tested <- numeric()
        for (j in 1:length(cutoffs)) {
          cutoff <- cutoffs[j]
          found[j] <- sensitivity(observed, pred, cutoff = cutoff, times = time)
          tested[j] <- mean(pred <= cutoff)
        }
        100 * data.frame(Found = c(0, found), Tested = c(0, tested))
      }),
      names = paste0("time", seq_along(times))
    )
  }
)
