#' Model Calibration
#' 
#' Calculate calibration estimates from observed and resampled response variable
#' values.
#' 
#' @param x Resamples object.
#' @param n number of resampled response variable bins within which to
#' calculate observed mean values.
#' @param ... arguments passed to other methods.
#' 
#' @return ResamplesCalibration class object.
#'  
#' @seealso \code{\link{resample}}, \code{\link{plot}}
#' 
#' @examples
#' library(survival)
#' 
#' perf <- resample(Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
#'                    pat.karno + meal.cal + wt.loss, data = lung,
#'                    model = GBMModel,
#'                    control = CVControl(surv_times = c(180, 360, 540)))
#' (cal <- calibration(perf))
#' plot(cal)
#' 
calibration <- function(x, n = 10, ...) {
  stopifnot(is(x, "Resamples"))
  if (length(dim(x)) > 2) {
    stop("calibration not available for multiple models")
  }
  
  .calibration(x@response$Observed, x@response$Predicted, n,
               times = x@control@surv_times) %>%
    structure(class = c("ResamplesCalibration", "data.frame"))
}


setGeneric(".calibration", function(observed, predicted, n, ...)
  standardGeneric(".calibration"))


setMethod(".calibration", c("ANY", "ANY"),
  function(observed, predicted, n, ...) {
    stop("calibration unavailable for response type")
  }
)


setMethod(".calibration", c("factor", "matrix"),
  function(observed, predicted, n, ...) {
    if (nlevels(observed) > 1) {
      observed <- stack(as.data.frame(model.matrix(~ observed - 1)))
      predicted <- stack(as.data.frame(predicted))
      df <- data.frame(Response = predicted$ind,
                       Midpoint = midpoints(predicted$values, n),
                       Observed = observed$values)
      aggregate(. ~ Response + Midpoint, df, function(x) {
        Mean <- mean(x)
        SE <- sd(x) / sqrt(length(x))
        c(Mean = Mean, SE = SE, Lower = max(Mean - SE, 0),
          Upper = min(Mean + SE, 1))
      })
    } else {
      .calibration(observed, predicted[, ncol(predicted)])
    }
  }
)


setMethod(".calibration", c("factor", "numeric"),
  function(observed, predicted, n, ...) {
    df <- data.frame(Response = factor("y"),
                     Midpoint = midpoints(predicted, n),
                     Observed = as.numeric(observed == levels(observed)[2]))
    aggregate(. ~ Response + Midpoint, df, function(x) {
        Mean <- mean(x)
        SE <- sd(x) / sqrt(length(x))
        c(Mean = Mean, SE = SE, Lower = max(Mean - SE, 0),
          Upper = min(Mean + SE, 1))
    })
  }
)


setMethod(".calibration", c("numeric", "ANY"),
  function(observed, predicted, n, ...) {
    df <- data.frame(Response = factor("y"),
                     Midpoint = midpoints(predicted, n),
                     Observed = observed)
    aggregate(. ~ Response + Midpoint, df, function(x) {
      Mean <- mean(x)
      SE <-  sd(x) / sqrt(length(x))
      c(Mean = Mean, SE = SE, Lower = Mean - SE, Upper = Mean + SE)
    })
  }
)


setMethod(".calibration", c("Surv", "matrix"),
  function(observed, predicted, n, times, ...) {
    num_obs <- nrow(predicted)
    colnames(predicted) <- paste0("Time", seq(times))
    predicted <- stack(as.data.frame(predicted))
    df <- data.frame(Response = predicted$ind,
                     Midpoint = midpoints(predicted$values, n),
                     Observed = rep(observed, times = length(times)),
                     Time = rep(times, each = num_obs))
    by_results <- by(df, df[c("Midpoint", "Response")], function(data) {
      km <- survfit(Observed ~ 1, data = data)
      interval <- findInterval(data$Time[1], c(0, km$time))
      Mean <- c(1, km$surv)[interval]
      SE <- c(0, km$std.err)[interval]
      result <- data[1, c("Response", "Midpoint")]
      result$Observed <- cbind(Mean = Mean, SE = SE, Lower = max(Mean - SE, 0),
                               Upper = min(Mean + SE, 1))
      result
    })
    do.call(rbind, by_results)
  }
)


midpoints <- function(x, n) {
  breakpoints <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = n + 1)
  midpoints <- head(breakpoints, -1) + diff(breakpoints) / 2
  intervals <- findInterval(x, breakpoints, rightmost.closed = TRUE)
  midpoints[intervals]
}
