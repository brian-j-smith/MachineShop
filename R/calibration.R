#' Model Calibration
#'
#' Calculate calibration estimates from observed and predicted responses.
#'
#' @name calibration
#' @rdname calibration
#'
#' @param x \link[=response]{observed responses} or \link{resample} result
#'   containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param breaks value defining the response variable bins within which to
#'   calculate observed mean values.  May be specified as a number of bins, a
#'   vector of breakpoints, or \code{NULL} to fit smooth curves with splines for
#'   predicted survival probabilities and with \link[stats:loess]{loess} for
#'   others.
#' @param span numeric parameter controlling the degree of loess smoothing.
#' @param dist character string specifying a distribution with which to estimate
#'   observed survival means.  Possible values are \code{"empirical"} for the
#'   Kaplan-Meier estimator, \code{"exponential"}, \code{"extreme"},
#'   \code{"gaussian"}, \code{"loggaussian"}, \code{"logistic"},
#'   \code{"loglogistic"}, \code{"lognormal"}, \code{"rayleigh"}, \code{"t"}, or
#'   \code{"weibull"} (default).
#' @param na.rm logical indicating whether to remove observed or predicted
#'   responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed to other methods.
#'
#' @return \code{Calibration} class object that inherits from \code{data.frame}.
#'
#' @seealso \code{\link{c}}, \code{\link{plot}}
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
calibration <- function(x, y = NULL, breaks = 10, span = 0.75, dist = NULL,
                        na.rm = TRUE, ...) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y)
    x <- complete$x
    y <- complete$y
  }
  .calibration(x, y, breaks = breaks, span = span, dist = dist)
}


Calibration <- function(object, ..., .check = TRUE) {
  if (.check) {
    if (is.null(object$Model)) object$Model <- factor("Model")
    missing <- missing_names(c("Response", "Predicted", "Observed"), object)
    if (length(missing)) {
      stop(label_items("missing calibration variable", missing))
    }
  }
  rownames(object) <- NULL
  new("Calibration", object, ...)
}


.calibration <- function(x, ...) {
  UseMethod(".calibration")
}


.calibration.default <- function(x, y, breaks, ...) {
  Calibration(.calibration_default(x, y, breaks = breaks, ...),
              smoothed = is.null(breaks))
}


.calibration.Resamples <- function(x, ...) {
  cal_list <- by(x, x$Model, function(data) {
    calibration(data$Observed, data$Predicted, na.rm = FALSE, ...)
  }, simplify = FALSE)
  do.call(c, cal_list)
}


setGeneric(".calibration_default", function(observed, predicted, ...)
  standardGeneric(".calibration_default"))


setMethod(".calibration_default", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("calibration unavailable for response type")
  }
)


setMethod(".calibration_default", c("factor", "matrix"),
  function(observed, predicted, ...) {
    cal <- calibration(model.matrix(~ observed - 1), predicted, ...)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration_default", c("factor", "numeric"),
  function(observed, predicted, ...) {
    observed <- as.numeric(observed == levels(observed)[2])
    cal <- calibration(observed, predicted, ...)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration_default", c("matrix", "matrix"),
  function(observed, predicted, breaks, span, ...) {
    df <- data.frame(Response = rep(colnames(predicted),
                                    each = nrow(predicted)),
                     Predicted = as.numeric(predicted))
    if (is.null(breaks)) {
      loessfit_list <- map(function(i) {
        y <- observed[, i]
        x <- predicted[, i]
        predict(loess(y ~ x, span = span), se = TRUE)
      }, 1:ncol(predicted))
      Mean <- c(map_num(getElement, loessfit_list, "fit"))
      SE <- c(map_num(getElement, loessfit_list, "se.fit"))
      df$Observed <- cbind(Mean = Mean, SE = SE,
                           Lower = Mean - SE,
                           Upper = Mean + SE)
      df
    } else {
      df$Predicted <- midpoints(df$Predicted, breaks)
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
  function(observed, predicted, ...) {
    calibration(cbind(y = observed), cbind(y = predicted), ...)
  }
)


setMethod(".calibration_default", c("Surv", "SurvProbs"),
  function(observed, predicted, breaks, ...) {
    times <- predicted@times
    df <- data.frame(Response = rep(colnames(predicted),
                                    each = nrow(predicted)),
                     Predicted = as.numeric(predicted))
    if (is.null(breaks)) {
      Mean <- c(map_num(function(i) {
        x <- predicted[, i]
        harefit <- polspline::hare(observed[, "time"], observed[, "status"], x)
        1 - polspline::phare(times[i], x, harefit)
      }, 1:ncol(predicted)))
      df$Observed <- cbind(Mean = Mean, SE = NA, Lower = NA, Upper = NA)
      df
    } else {
      df$Predicted <- midpoints(df$Predicted, breaks)
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
  function(observed, predicted, breaks, dist, span, ...) {
    max_time <- surv_max(observed)
    dist <- if (is.null(dist)) {
      MachineShop::settings("dist.Surv")
    } else {
      match.arg(dist, c("empirical", names(survreg.distributions)))
    }
    nparams <- if (dist %in% c("exponential", "rayleigh")) 1 else 2

    f_survfit <- function(observed, weights = NULL) {
      km <- survfit(observed ~ 1, weights = weights, se.fit = FALSE)
      est <- survival:::survmean(km, rmean = max_time)
      list(Mean = est$matrix[["*rmean"]], SE = est$matrix[["*se(rmean)"]])
    }

    f_survreg <- function(observed, dist, weights = NULL) {
      regfit <- survreg(observed ~ 1, weights = weights, dist = dist)
      est <- predict(regfit, data.frame(row.names = 1), se.fit = TRUE)
      list(Mean = est$fit[[1]], SE = est$se.fit[[1]])
    }

    if (is.null(breaks)) {
      df <- data.frame(
        Response = "Mean",
        Predicted = unique(predicted)
      )
      tricubic <- function(x, span = 1, min_weight = 0) {
        x <- abs(x)
        x_range <- span * diff(range(x))
        (1 - min_weight) * pmax((1 - (x / x_range)^3)^3, 0) + min_weight
      }
      metrics_list <- map(function(value) {
        weights <- tricubic(predicted - value, span = span, min_weight = 0.01)
        est <- if (dist == "empirical") {
          f_survfit(observed, weights)
        } else {
          f_survreg(observed, dist, weights)
        }
        with(est, c(Mean = Mean, SE = SE,
                    Lower = max(Mean - SE, 0),
                    Upper = Mean + SE))
      }, df$Predicted)
      df$Observed <- do.call(rbind, metrics_list)
      df
    } else {
      df <- data.frame(
        Response = "Mean",
        Predicted = midpoints(predicted, breaks),
        Observed = observed
      )
      by_results <- by(df, df[c("Predicted", "Response")], function(data) {
        observed <- data$Observed
        est <- if (dist == "empirical") {
          f_survfit(observed)
        } else if (length(surv_times(observed)) >= nparams) {
          f_survreg(observed, dist)
        } else {
          list(Mean = NA_real_, SE = NA_real_)
        }
        result <- data[1, c("Response", "Predicted")]
        result$Observed <- with(est, cbind(Mean = Mean, SE = SE,
                                           Lower = max(Mean - SE, 0),
                                           Upper = Mean + SE))
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
