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
#' calculate observed mean values.  May be specified as a number of bins, a
#' vector of breakpoints, or \code{NULL} to fit smooth curves with splines for
#' survival responses and loess for others.
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
#'                 control = CVControl(times = 365 * c(2, 5, 10)))
#' cal <- calibration(res)
#' plot(cal)
#' 
calibration <- function(x, y = NULL, breaks = 10, ...) {
  .calibration(x, y, breaks = breaks)
}


.calibration <- function(x, ...) {
  UseMethod(".calibration")
}


.calibration.default <- function(x, y, breaks, ...) {
  Calibration(.calibration_default(x, y, breaks = breaks), .breaks = breaks)
}


.calibration.Resamples <- function(x, breaks, ...) {
  cal_list <- by(x, x$Model, function(data) {
    calibration(data$Observed, data$Predicted, breaks = breaks)
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
    df <- data.frame(Response = rep(colnames(predicted),
                                    each = nrow(predicted)))
    if (is.null(breaks)) {
      df$Predicted <- c(predicted)
      loessfit_list <- lapply(1:ncol(predicted), function(i) {
        y <- observed[, i]
        x <- predicted[, i]
        predict(loess(y ~ x), se = TRUE)
      })
      Mean <- c(sapply(loessfit_list, getElement, name = "fit"))
      SE <- c(sapply(loessfit_list, getElement, name = "se.fit"))
      df$Observed <- cbind(Mean = Mean, SE = SE,
                           Lower = Mean - SE,
                           Upper = Mean + SE)
      df
    } else {
      df$Predicted <- midpoints(c(predicted), breaks)
      df$Observed <- c(observed)
      aggregate(. ~ Response + Predicted, df, function(x) {
        Mean <- mean(x)
        SE <- sd(x) / sqrt(length(x))
        c(Mean = Mean, SE = SE, Lower = Mean - SE, Upper = Mean + SE)
      })
    }
  }
)


setMethod(".calibration_default", c("numeric", "numeric"),
  function(observed, predicted, breaks, ...) {
    calibration(cbind(y = observed), cbind(y = predicted), breaks = breaks)
  }
)


setMethod(".calibration_default", c("Surv", "SurvProbs"),
  function(observed, predicted, breaks, ...) {
    times <- predicted@times
    colnames(predicted) <- paste("Time", 1:length(times))
    df <- data.frame(Response = rep(colnames(predicted),
                                    each = nrow(predicted)))
    if (is.null(breaks)) {
      df$Predicted <- c(predicted)
      Mean <- c(sapply(1:ncol(predicted), function(i) {
        x <- predicted[, i, drop = FALSE]
        harefit <- polspline::hare(observed[, "time"], observed[, "status"], x)
        1 - polspline::phare(times[i], x, harefit)
      }))
      df$Observed <- cbind(Mean = Mean, SE = NA, Lower = NA, Upper = NA)
      df
    } else {
      df$Predicted <- midpoints(c(predicted), breaks)
      df$Observed <- rep(observed, times = length(times))
      df$Time <- rep(times, each = nrow(predicted))
      by_results <- by(df, df[c("Predicted", "Response")], function(data) {
        km <- survfit(Observed ~ 1, data = data)
        interval <- findInterval(data$Time[1], c(0, km$time))
        Mean <- c(1, km$surv)[interval]
        SE <- c(0, km$std.err)[interval]
        result <- data[1, c("Response", "Predicted")]
        result$Observed <- cbind(Mean = Mean, SE = SE,
                                 Lower = max(Mean - SE, 0),
                                 Upper = min(Mean + SE, 1))
        result
      }, simplify = FALSE)
      do.call(rbind, by_results)
    }
  }
)


setMethod(".calibration_default", c("Surv", "numeric"),
  function(observed, predicted, breaks, ...) {
    surv_max <- surv_max(observed)
    if (is.null(breaks)) {
      df <- data.frame(
        Response = "Mean",
        Predicted = unique(predicted)
      )
      metrics_list <- lapply(df$Predicted, function(value) {
        abs_diff <- abs(predicted - value)
        weights <- (1 - (abs_diff / diff(range(abs_diff)))^3)^3
        km <- survfit(observed ~ 1, weights = weights, se.fit = FALSE)
        c(Mean = surv_mean(km$time, km$surv, max_time = surv_max),
          SE = NA, Lower = NA, Upper = NA)
      })
      df$Observed <- do.call(rbind, metrics_list)
      df
    } else {
      df <- data.frame(
        Response = "Mean",
        Predicted = midpoints(predicted, breaks),
        Observed = observed
      )
      by_results <- by(df, df[c("Predicted", "Response")], function(data) {
        km <- survfit(Observed ~ 1, data = data, se.fit = FALSE)
        est <- survival:::survmean(km, rmean = surv_max)
        Mean <- est$matrix["*rmean"]
        SE <- est$matrix["*se(rmean)"]
        result <- data[1, c("Response", "Predicted")]
        result$Observed <- cbind(Mean = Mean, SE = SE,
                                 Lower = max(Mean - SE, 0),
                                 Upper = Mean + SE)
        result
      }, simplify = FALSE)
      do.call(rbind, by_results)
    }
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
