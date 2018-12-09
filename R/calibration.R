#' Model Calibration
#' 
#' Calculate calibration estimates from observed and predicted responses.
#' 
#' @param x observed responses or \code{Resamples} object of observed and
#' predicted responses.
#' @param y predicted responses.
#' @param n number of resampled response variable bins within which to
#' calculate observed mean values.
#' @param times numeric vector of follow-up times if \code{y} contains predicted
#' survival events.
#' @param ... arguments passed to other methods.
#' 
#' @return \code{Calibration} class object that inherits from \code{data.frame}.
#'  
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{plot}}
#' 
#' @examples
#' library(survival)
#' library(MASS)
#' 
#' res <- resample(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'                 data = Melanoma, model = GBMModel,
#'                 control = CVControl(surv_times = 365 * c(2, 5, 10)))
#' (cal <- calibration(res))
#' plot(cal)
#' 
calibration <- function(x, y = NULL, n = 10, times = numeric(), ...) {
  .calibration(x, y, n = n, times = times)
}


.calibration <- function(x, ...) {
  UseMethod(".calibration")
}


.calibration.default <- function(x, y, n, times, ...) {
  Calibration(.calibration_default(x, y, n = n, times = times))
}


.calibration.Resamples <- function(x, n, ...) {
  times <- x@control@surv_times
  cal_list <- by(x, x$Model, function(data) {
    cal <- calibration(data$Observed, data$Predicted, n = n, times = times)
    cal$Model <- as.character(data$Model[1])
    cal
  }, simplify = FALSE)
  do.call(Calibration, cal_list)
}


setGeneric(".calibration_default", function(observed, predicted, ...)
  standardGeneric(".calibration_default"))


setMethod(".calibration_default", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("calibration unavailable for response type")
  }
)


setMethod(".calibration_default", c("factor", "matrix"),
  function(observed, predicted, n, ...) {
    cal <- calibration(model.matrix(~ observed - 1), predicted, n = n)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration_default", c("factor", "numeric"),
  function(observed, predicted, n, ...) {
    cal <-
      calibration(as.numeric(observed == levels(observed)[2]), predicted, n = n)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration_default", c("matrix", "matrix"),
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


setMethod(".calibration_default", c("numeric", "numeric"),
  function(observed, predicted, n, ...) {
    calibration(cbind(y = observed), cbind(y = predicted), n = n)
  }
)


setMethod(".calibration_default", c("Surv", "matrix"),
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
