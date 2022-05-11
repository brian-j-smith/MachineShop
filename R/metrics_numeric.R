#' @rdname metrics
#'
gini <- function(observed, predicted = NULL, weights = NULL, ...) {
  call_metric_method("gini", environment())
}

MLMetric(gini) <- list("gini", "Gini Coefficient", FALSE)


setMetric_numeric("gini",
  function(observed, predicted, weights, ...) {
    weights <- check_weights(weights, observed)
    throw(check_assignment(weights))
    f <- function(x) {
      sort_order <- order(x, weights)
      y <- observed[sort_order]
      w <- weights[sort_order]
      n <- length(x)

      y <- w * y
      y <- cumsum(y)
      y <- y / y[n]

      w <- cumsum(w)
      w <- w / w[n]

      c(y[-1] %*% w[-n] - y[-n] %*% w[-1])
    }
    f(predicted) / f(observed)
  }
)


#' @rdname metrics
#'
mae <- function(observed, predicted = NULL, weights = NULL, ...) {
  call_metric_method("mae", environment())
}

MLMetric(mae) <- list("mae", "Mean Absolute Error", FALSE)


setMetric_numeric("mae",
  function(observed, predicted, weights, ...) {
    weighted_mean(abs(observed - predicted), weights)
  }
)


#' @rdname metrics
#'
mse <- function(observed, predicted = NULL, weights = NULL, ...) {
  call_metric_method("mse", environment())
}

MLMetric(mse) <- list("mse", "Mean Squared Error", FALSE)


setMetric_numeric("mse",
  function(observed, predicted, weights, ...) {
    weighted_mean((observed - predicted)^2, weights)
  }
)


#' @rdname metrics
#'
msle <- function(observed, predicted = NULL, weights = NULL, ...) {
  call_metric_method("msle", environment())
}

MLMetric(msle) <- list("msle", "Mean Squared Log Error", FALSE)


setMetric_numeric("msle",
  function(observed, predicted, weights, ...) {
    mse(log(1 + observed), log(1 + predicted), weights)
  }
)


#' @rdname metrics
#'
r2 <- function(
  observed, predicted = NULL, weights = NULL,
  method = c("mse", "pearson", "spearman"), distr = character(), ...
) {
  method <- match.arg(method)
  call_metric_method("r2", environment())
}

MLMetric(r2) <- list("r2", "Coefficient of Determination", TRUE)


setMetricGeneric("r2")


setMetricMethod_BinomialVariate("r2")


setMetricMethod_matrix("r2")


setMetricMethod("r2", c("numeric", "numeric"),
  function(observed, predicted, weights, method, ...) {
    if (method == "mse") {
      obs_mean <- weighted_mean(observed, weights)
      1 - mse(observed, predicted, weights) / mse(observed, obs_mean, weights)
    } else {
      if (method == "spearman") {
        observed <- rank(observed)
        predicted <- rank(predicted)
      }
      weights <- check_weights(weights, observed)
      throw(check_assignment(weights))
      cov.wt(cbind(observed, predicted), wt = weights, cor = TRUE)$cor[2]^2
    }
  }
)


setMetricMethod_Resample("r2")


setMetricMethod("r2", c("Surv", "numeric"),
  function(observed, predicted, weights, method, distr, ...) {
    if (method == "mse") {
      distr <- get_surv_distr(distr, observed, predicted)
      nparams <- if (distr %in% c("exponential", "rayleigh")) 1 else 2
      obs_mean <- if (distr == "empirical") {
        surv <- survfit(observed ~ 1, weights = weights, se.fit = FALSE)
        rep(mean(surv), length(observed))
      } else if (length(event_time(observed)) >= nparams) {
        predict(survreg(observed ~ 1, weights = weights, dist = distr))
      } else {
        rep(NA_real_, length(observed))
      }
      1 - mse(observed, predicted, weights) / mse(observed, obs_mean, weights)
    } else {
      metric_SurvTimes(r2, observed, predicted, weights, method = method)
    }
  }
)


#' @rdname metrics
#'
rmse <- function(observed, predicted = NULL, weights = NULL, ...) {
  call_metric_method("rmse", environment())
}

MLMetric(rmse) <- list("rmse", "Root Mean Squared Error", FALSE)


setMetric_numeric("rmse",
  function(observed, predicted, weights, ...) {
    sqrt(mse(observed, predicted, weights))
  }
)


#' @rdname metrics
#'
rmsle <- function(observed, predicted = NULL, weights = NULL, ...) {
  call_metric_method("rmsle", environment())
}

MLMetric(rmsle) <- list("rmsle", "Root Mean Squared Log Error", FALSE)


setMetric_numeric("rmsle",
  function(observed, predicted, weights, ...) {
    sqrt(msle(observed, predicted, weights))
  }
)
