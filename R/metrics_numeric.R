#' @rdname metrics
#'
gini <- function(observed, predicted = NULL, ...) {
  call_metric_method("gini", environment())
}

MLMetric(gini) <- list("gini", "Gini Coefficient", FALSE)


setMetric_numeric("gini",
  function(observed, predicted, ...) {
    gini_sum <- function(x) {
      n <- length(x)
      y <- observed[order(x)]
      n + 1 - 2 * sum((n:1) * y) / sum(y)
    }
    gini_sum(predicted) / gini_sum(observed)
  }
)


#' @rdname metrics
#'
mae <- function(observed, predicted = NULL, ...) {
  call_metric_method("mae", environment())
}

MLMetric(mae) <- list("mae", "Mean Absolute Error", FALSE)


setMetric_numeric("mae",
  function(observed, predicted, ...) {
    mean(abs(observed - predicted))
  }
)


#' @rdname metrics
#'
mse <- function(observed, predicted = NULL, ...) {
  call_metric_method("mse", environment())
}

MLMetric(mse) <- list("mse", "Mean Squared Error", FALSE)


setMetric_numeric("mse",
  function(observed, predicted, ...) {
    mean((observed - predicted)^2)
  }
)


#' @rdname metrics
#'
msle <- function(observed, predicted = NULL, ...) {
  call_metric_method("msle", environment())
}

MLMetric(msle) <- list("msle", "Mean Squared Log Error", FALSE)


setMetric_numeric("msle",
  function(observed, predicted, ...) {
    mean((log(1 + observed) - log(1 + predicted))^2)
  }
)


#' @rdname metrics
#'
r2 <- function(observed, predicted = NULL, dist = NULL, ...) {
  call_metric_method("r2", environment())
}

MLMetric(r2) <- list("r2", "Coefficient of Determination", TRUE)


setMetricGeneric("r2")


setMetricMethod_BinomialMatrix_numeric("r2")


setMetricMethod_matrix_matrix("r2")


setMetricMethod("r2", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    1 - mse(observed, predicted) / mse(observed, mean(observed))
  }
)


setMetricMethod_Resamples("r2")


setMetricMethod("r2", c("Surv", "numeric"),
  function(observed, predicted, dist, ...) {
    dist <- if (is.null(dist)) {
      settings("dist.Surv")
    } else {
      match.arg(dist, c("empirical", names(survreg.distributions)))
    }
    nparams <- if (dist %in% c("exponential", "rayleigh")) 1 else 2
    observed_mean <- if (dist == "empirical") {
      rep(mean(survfit(observed ~ 1, se.fit = FALSE)), length(observed))
    } else if (length(surv_times(observed)) >= nparams) {
      predict(survreg(observed ~ 1, dist = dist))
    } else {
      rep(NA_real_, length(observed))
    }
    1 - mse(observed, predicted) / mse(observed, observed_mean)
  }
)


#' @rdname metrics
#'
rmse <- function(observed, predicted = NULL, ...) {
  call_metric_method("rmse", environment())
}

MLMetric(rmse) <- list("rmse", "Root Mean Squared Error", FALSE)


setMetric_numeric("rmse",
  function(observed, predicted, ...) {
    sqrt(mse(observed, predicted))
  }
)


#' @rdname metrics
#'
rmsle <- function(observed, predicted = NULL, ...) {
  call_metric_method("rmsle", environment())
}

MLMetric(rmsle) <- list("rmsle", "Root Mean Squared Log Error", FALSE)


setMetric_numeric("rmsle",
  function(observed, predicted, ...) {
    sqrt(msle(observed, predicted))
  }
)
