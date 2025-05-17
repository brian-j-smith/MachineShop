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
#' @param weights numeric vector of non-negative
#'   \link[=case_weights]{case weights} for the observed \code{x} responses
#'   [default: equal weights].
#' @param breaks value defining the response variable bins within which to
#'   calculate observed mean values.  May be specified as a number of bins, a
#'   vector of breakpoints, or \code{NULL} to fit smooth curves with splines for
#'   predicted survival probabilities and with \link[stats:loess]{loess} for
#'   others.
#' @param span numeric parameter controlling the degree of loess smoothing.
#' @param distr character string specifying a distribution with which to
#'   estimate the observed survival mean.  Possible values are
#'   \code{"empirical"} for the Kaplan-Meier estimator, \code{"exponential"},
#'   \code{"extreme"}, \code{"gaussian"}, \code{"loggaussian"},
#'   \code{"logistic"}, \code{"loglogistic"}, \code{"lognormal"},
#'   \code{"rayleigh"}, \code{"t"}, or \code{"weibull"}.  Defaults to the
#'   distribution that was used in predicting mean survival times.
#' @param pool logical indicating whether to compute a single calibration curve
#'   on predictions pooled over all resampling iterations or to compute them for
#'   each iteration individually and return the mean calibration curve. Pooling
#'   can result in large memory allocation errors when fitting smooth curves
#'   with \code{breaks = NULL}.
#' @param na.rm logical indicating whether to remove observed or predicted
#'   responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed to other methods.
#'
#' @return \code{Calibration} class object that inherits from \code{data.frame}.
#'
#' @seealso \code{\link{c}}, \code{\link{plot}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' library(survival)
#'
#' control <- CVControl() %>% set_predict(times = c(90, 180, 360))
#' res <- resample(Surv(time, status) ~ ., data = veteran, model = GBMModel,
#'                 control = control)
#' cal <- calibration(res)
#' plot(cal)
#' }
#'
calibration <- function(
  x, y = NULL, weights = NULL, breaks = 10, span = 0.75, distr = character(),
  pool = TRUE, na.rm = TRUE, ...
) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y, weights = weights)
    x <- complete$x
    y <- complete$y
    weights <- complete$weights
  }
  Calibration(
    .calibration(
      x, y, weights, breaks = breaks, span = span, distr = distr, pool = pool,
      ...
    ),
    smoothed = is_empty(breaks)
  )
}


Calibration <- function(object, ..., .check = TRUE) {
  if (.check) {
    if (is.null(object$Model)) object$Model <- factor("Model")
    missing <- missing_names(c("Response", "Predicted", "Observed"), object)
    if (length(missing)) {
      throw(Error(note_items(
        "Missing calibration variable{?s}: ", missing, "."
      )))
    }
  }
  rownames(object) <- NULL
  new("Calibration", object, ...)
}


setGeneric(".calibration",
  function(observed, predicted, ...) standardGeneric(".calibration")
)


setMethod(".calibration", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    throw(Error("Calibration unavailable for response type."))
  }
)


setMethod(".calibration", c("factor", "matrix"),
  function(observed, predicted, ...) {
    cal <- .calibration(model.matrix(~ observed - 1), predicted, ...)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration", c("factor", "numeric"),
  function(observed, predicted, ...) {
    observed <- as.numeric(observed == levels(observed)[2])
    cal <- .calibration(observed, predicted, ...)
    bounds <- c("Lower", "Upper")
    cal$Observed[, bounds] <- pmin(pmax(cal$Observed[, bounds], 0), 1)
    cal
  }
)


setMethod(".calibration", c("matrix", "matrix"),
  function(observed, predicted, weights, breaks, span, xlim = NULL, ...) {
    weights <- check_weights(weights, observed[, 1])
    throw(check_assignment(weights))
    if (is_empty(breaks)) {
      x <- predicted
      newx <- if (!is_empty(xlim)) {
        matrix(seq(xlim[1], xlim[2], length = 101), 101, ncol(x))
      } else x
      res <- data.frame(
        Response = rep(factor(colnames(x)), each = nrow(newx)),
        Predicted = as.numeric(newx)
      )
      loess_fits <- map(function(col) {
        data <- data.frame(y = observed[, col], x = x[, col])
        newdata <- data.frame(x = newx[, col])
        predict(
          loess(y ~ x, data = data, weights = weights, span = span),
          newdata = newdata, se = TRUE
        )
      }, seq_len(ncol(x)))
      Mean <- c(map("num", getElement, loess_fits, "fit"))
      SE <- c(map("num", getElement, loess_fits, "se.fit"))
      res$Observed <- cbind(
        Mean = Mean, SE = SE, Lower = Mean - SE, Upper = Mean + SE
      )
      res
    } else {
      df <- data.frame(
        Response = rep(factor(colnames(predicted)), each = nrow(predicted)),
        Predicted = midpoints(as.numeric(predicted), breaks, xlim = xlim),
        Observed = c(observed),
        Weight = weights
      )
      res <- rev(expand.grid(
        Predicted = levels(df$Predicted),
        Response = unique(df$Response)
      ))
      res$Observed <- matrix(NA, nrow(res), 4)
      colnames(res$Observed) <- c("Mean", "SE", "Lower", "Upper")
      data_splits <- split(df, df[c("Response", "Predicted")], drop = TRUE)
      for (data in data_splits) {
        Mean <- weighted_mean(data$Observed, data$Weight)
        SE <- weighted_sd(data$Observed, data$Weight) / sqrt(nrow(data))
        ind <- res$Response == data$Response[1] &
          res$Predicted == data$Predicted[1]
        res[ind, "Observed"] <- cbind(Mean, SE, Mean - SE, Mean + SE)
      }
      res
    }
  }
)


setMethod(".calibration", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    .calibration(cbind(y = observed), cbind(y = predicted), ...)
  }
)


setMethod(".calibration", c("Resample", "ANY"),
  function(observed, predicted, weights, pool, ...) {
    cal_model <- by(observed, observed$Model, function(resample) {
      if (pool) {
        calibration(
          resample$Observed, resample$Predicted, resample$Weight, na.rm = FALSE,
          ...
        )
      } else {
        xlim <- range(resample$Predicted, finite = TRUE)
        cal_iter <- by(resample, resample$Iteration, function(resample_iter) {
          res <- calibration(
            resample_iter$Observed, resample_iter$Predicted,
            resample_iter$Weight, na.rm = FALSE, xlim = xlim, ...
          )
          res$Observed <- res$Observed[, "Mean"]
          res
        })
        get_stats <- function(col, fun) {
          num_mat <- map("num", function(x) x[, col], cal_iter)
          apply(num_mat, 1, function(x) fun(na.omit(x)))
        }
        cal_iter1 <- cal_iter[[1]]
        cal_iter1$Predicted <- get_stats("Predicted", mean)
        Mean <- get_stats("Observed", mean)
        SE <- get_stats("Observed", sd)
        cal_iter1$Observed <- cbind(
          Mean = Mean, SE = SE, Lower = Mean - SE, Upper = Mean + SE
        )
        cal_iter1
      }
    }, simplify = FALSE)
    do.call(c, cal_model)
  }
)


setMethod(".calibration", c("Surv", "SurvProbs"),
  function(observed, predicted, weights, breaks, xlim = NULL, ...) {
    weights <- check_weights(weights, observed)
    throw(check_assignment(weights))
    times <- predicted@times
    if (is_empty(breaks)) {
      throw(check_censoring(observed, "right"))
      throw(check_equal_weights(weights))
      x <- predicted
      newx <- if (!is_empty(xlim)) {
        matrix(seq(xlim[1], xlim[2], length = 101), ncol = ncol(x))
      } else x
      res <- data.frame(
        Response = rep(factor(colnames(x)), each = nrow(newx)),
        Predicted = as.numeric(newx)
      )
      Mean <- c(map("num", function(col) tryCatch(
        {
          harefit <- polspline::hare(
            observed[, "time"], observed[, "status"], x[, col]
          )
          1 - polspline::phare(times[col], newx[, col], harefit)
        },
        error = function(cond) {
          throw(LocalWarning(conditionMessage(cond)))
          rep(NA, nrow(newx))
        }
      ), seq_len(ncol(x))))
      res$Observed <- cbind(Mean = Mean, SE = NA, Lower = NA, Upper = NA)
      res
    } else {
      df <- data.frame(
        Response = rep(factor(colnames(predicted)), each = nrow(predicted)),
        Predicted = midpoints(as.numeric(predicted), breaks, xlim = xlim),
        Observed = rep(observed, times = length(times)),
        Weight = weights,
        Time = rep(times, each = nrow(predicted))
      )
      res <- rev(expand.grid(
        Predicted = levels(df$Predicted),
        Response = unique(df$Response)
      ))
      res$Observed <- matrix(NA, nrow(res), 4)
      colnames(res$Observed) <- c("Mean", "SE", "Lower", "Upper")
      data_splits <- split(df, df[c("Response", "Predicted")], drop = TRUE)
      for (data in data_splits) {
        km <- survfit(Observed ~ 1, data = data, weights = data$Weight)
        inds <- findInterval(data$Time[1], c(0, km$time))
        Mean <- c(1, km$surv)[inds]
        SE <- c(0, km$std.err)[inds]
        ind <- res$Response == data$Response[1] &
          res$Predicted == data$Predicted[1]
        res[ind, "Observed"] <- cbind(
          Mean, SE, max(Mean - SE, 0), min(Mean + SE, 1)
        )
      }
      res
    }
  }
)


setMethod(".calibration", c("Surv", "numeric"),
  function(observed, predicted, weights, breaks, distr, span, xlim = NULL, ...)
  {
    weights <- check_weights(weights, observed)
    throw(check_assignment(weights))
    max_time <- max(time(observed))
    distr <- get_surv_distr(distr, observed, predicted)
    nparams <- if (distr %in% c("exponential", "rayleigh")) 1 else 2

    survfit_est <- function(observed, weights = NULL) {
      km <- survfit(observed ~ 1, weights = weights, se.fit = FALSE)
      est <- survival:::survmean(km, rmean = max_time)
      list(Mean = est$matrix[["*rmean"]], SE = est$matrix[["*se(rmean)"]])
    }

    survreg_est <- function(observed, distr, weights = NULL) {
      regfit <- survreg(observed ~ 1, weights = weights, dist = distr)
      est <- predict(regfit, data.frame(row.names = 1), se.fit = TRUE)
      list(Mean = est$fit[[1]], SE = est$se.fit[[1]])
    }

    if (is_empty(breaks)) {
      res <- data.frame(
        Response = factor("Mean"),
        Predicted = if (is_empty(xlim)) {
          unique(predicted)
        } else {
          seq(xlim[1], xlim[2], length = 101)
        }
      )
      tricubic <- function(x, span = 1, min_weight = 0) {
        x <- abs(x)
        x_range <- span * diff(range(x))
        (1 - min_weight) * pmax((1 - (x / x_range)^3)^3, 0) + min_weight
      }
      surv_ests <- map(function(value) {
        weights <- weights *
          tricubic(predicted - value, span = span, min_weight = 0.01)
        est <- if (distr == "empirical") {
          survfit_est(observed, weights)
        } else {
          survreg_est(observed, distr, weights)
        }
        with(est, {
          c(Mean = Mean, SE = SE, Lower = max(Mean - SE, 0), Upper = Mean + SE)
        })
      }, res$Predicted)
      res$Observed <- do.call(rbind, surv_ests)
      res
    } else {
      df <- data.frame(
        Response = factor("Mean"),
        Predicted = midpoints(predicted, breaks, xlim = xlim),
        Observed = observed,
        Weight = weights
      )
      res <- rev(expand.grid(
        Predicted = levels(df$Predicted),
        Response = unique(df$Response)
      ))
      res$Observed <- matrix(NA, nrow(res), 4)
      colnames(res$Observed) <- c("Mean", "SE", "Lower", "Upper")
      data_splits <- split(df, df[c("Response", "Predicted")], drop = TRUE)
      for (data in data_splits) {
        observed <- data$Observed
        weights <- data$Weight
        est <- if (distr == "empirical") {
          survfit_est(observed, weights)
        } else if (length(event_time(observed)) >= nparams) {
          survreg_est(observed, distr, weights)
        } else {
          list(Mean = NA_real_, SE = NA_real_)
        }
        ind <- res$Response == data$Response[1] &
          res$Predicted == data$Predicted[1]
        res[ind, "Observed"] <- with(est, {
          cbind(Mean, SE, max(Mean - SE, 0), min(Mean + SE, 1))
        })
      }
      res
    }
  }
)


midpoints <- function(x, breaks, xlim = NULL) {
  breaks <- if (length(breaks) == 1) {
    break_range <- if (is_empty(xlim)) range(x, finite = TRUE) else xlim
    num_breaks <- max(as.integer(breaks), 1) + 1
    seq(break_range[1], break_range[2], length = num_breaks)
  } else {
    sort(breaks)
  }
  levels <- breaks[-length(breaks)] + diff(breaks) / 2
  structure(
    levels[.bincode(x, breaks, include.lowest = TRUE)],
    levels = levels
  )
}
