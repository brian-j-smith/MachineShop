#' Model Calibration
#' 
#' Calculate calibration estimates from observed and predicted responses.
#' 
#' @rdname calibration
#' 
#' @param x observed responses or \code{Resamples} object of observed and
#' predicted responses.
#' @param y predicted responses.
#' @param breaks value defining the response variable bins within which to
#' calculate observed mean values.  May be specified as a number of bins or a
#' vector of breakpoints.
#' @param times numeric vector of follow-up times if \code{y} contains predicted
#' survival events.
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
calibration <- function(x, y = NULL, breaks = 10, times = numeric(), ...) {
  calibration_depwarn(...)
  .calibration(x, y, breaks = breaks, times = times)
}


calibration_depwarn <- function(n = NULL, ...) {
  if (!is.null(n)) {
    depwarn("'n' argument to calibration is deprecated",
            "use 'breaks' instead")
  }
}


.calibration <- function(x, ...) {
  UseMethod(".calibration")
}


.calibration.default <- function(x, y, breaks, times, ...) {
  Calibration(.calibration_default(x, y, breaks = breaks, times = times))
}


.calibration.Resamples <- function(x, breaks, ...) {
  times <- x@control@surv_times
  cal_list <- by(x, x$Model, function(data) {
    calibration(data$Observed, data$Predicted, breaks = breaks, times = times)
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
  function(observed, predicted, breaks, ...) {
    cal <- calibration(model.matrix(~ observed - 1), predicted, breaks = breaks)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration_default", c("factor", "numeric"),
  function(observed, predicted, breaks, ...) {
    cal <- calibration(as.numeric(observed == levels(observed)[2]), predicted,
                       breaks = breaks)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration_default", c("matrix", "matrix"),
  function(observed, predicted, breaks, ...) {
    observed <- stack(as.data.frame(observed))
    predicted <- stack(as.data.frame(predicted))
    df <- data.frame(Response = predicted$ind,
                     Midpoint = midpoints(predicted$values, breaks),
                     Observed = observed$values)
    aggregate(. ~ Response + Midpoint, df, function(x) {
      Mean <- mean(x)
      SE <- sd(x) / sqrt(length(x))
      c(Mean = Mean, SE = SE, Lower = Mean - SE, Upper = Mean + SE)
    })
  }
)


setMethod(".calibration_default", c("numeric", "numeric"),
  function(observed, predicted, breaks, ...) {
    calibration(cbind(y = observed), cbind(y = predicted), breaks = breaks)
  }
)


setMethod(".calibration_default", c("Surv", "matrix"),
  function(observed, predicted, breaks, times, ...) {
    num_obs <- nrow(predicted)
    colnames(predicted) <- paste0("Time", seq(times))
    predicted <- stack(as.data.frame(predicted))
    df <- data.frame(Response = predicted$ind,
                     Midpoint = midpoints(predicted$values, breaks),
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


midpoints <- function(x, breaks) {
  breaks <- if (length(breaks) == 1) {
    break_range <- range(x, na.rm = TRUE)
    num_breaks <- max(as.integer(breaks), 1) + 1
    seq(break_range[1], break_range[2], length = num_breaks)
  } else {
    sort(breaks) 
  }
  mids <- breaks[-length(breaks)] + diff(breaks) / 2
  mids[.bincode(x, breaks, include.lowest = TRUE)]
}
