#' Model Calibration
#' 
#' Calculate calibration estimates from observed and resampled response variable
#' values.
#' 
#' @param x \code{Resamples} object.
#' @param n number of resampled response variable bins within which to
#' calculate observed mean values.
#' @param ... arguments passed to other methods.
#' 
#' @return \code{CalibrationResamples} class object that inherits from
#' \code{data.frame}.
#'  
#' @seealso \code{\link{resample}}, \code{\link{plot}}
#' 
#' @examples
#' library(survival)
#' library(MASS)
#' 
#' perf <- resample(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'                  data = Melanoma, model = GBMModel,
#'                  control = CVControl(surv_times = 365 * c(2, 5, 10)))
#' (cal <- calibration(perf))
#' plot(cal)
#' 
calibration <- function(x, n = 10, ...) {
  stopifnot(is(x, "Resamples"))
  
  times <- x@control@surv_times
  
  cal_list <- by(response(x), response(x)$Model, function(data) {
    .calibration(data$Observed, data$Predicted, n, times = times) %>%
      cbind(Model = data$Model[1])
  }, simplify = FALSE)

  structure(do.call(rbind, cal_list),
            class = c("CalibrationResamples", "data.frame"))
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
    cal <- .calibration(model.matrix(~ observed - 1), predicted, n)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration", c("factor", "numeric"),
  function(observed, predicted, n, ...) {
    cal <-
      .calibration(as.numeric(observed == levels(observed)[2]), predicted, n)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration", c("matrix", "matrix"),
  function(observed, predicted, n, ...) {
    observed <- stack(as.data.frame(observed))
    predicted <- stack(as.data.frame(predicted))
    df <- data.frame(Response = predicted$ind,
                     Midpoint = midpoints(predicted$values, n),
                     Observed = observed$values)
    aggregate(. ~ Response + Midpoint, df, function(x) {
      Mean <- mean(x)
      SE <- sd(x) / sqrt(length(x))
      c(Mean = Mean, SE = SE, Lower = Mean - SE, Upper = Mean + SE)
    })
  }
)


setMethod(".calibration", c("numeric", "numeric"),
  function(observed, predicted, n, ...) {
    .calibration(cbind(y = observed), cbind(y = predicted), n)
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
    }, simplify = FALSE)
    do.call(rbind, by_results)
  }
)


midpoints <- function(x, n) {
  breakpoints <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = n + 1)
  midpoints <- head(breakpoints, -1) + diff(breakpoints) / 2
  intervals <- findInterval(x, breakpoints, rightmost.closed = TRUE)
  midpoints[intervals]
}
