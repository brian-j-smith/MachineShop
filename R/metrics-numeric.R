#' @rdname metrics
#' 
gini <- function(observed, predicted = NULL, ...) {
  .gini(observed, predicted)
}

MLMetric(gini) <- list("gini", "Gini Coefficient", FALSE)


setGeneric(".gini",
           function(observed, predicted, ...) standardGeneric(".gini"))


setMethod(".gini", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".gini", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    .metric.matrix(observed, predicted, gini)
  }
)


setMethod(".gini", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    gini_sum <- function(x) {
      n <- length(x)
      y <- observed[order(x)]
      n + 1 - 2 * sum((n:1) * y) / sum(y)
    }
    gini_sum(predicted) / gini_sum(observed)
  }
)


setMethod(".gini", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.SurvMean(observed, predicted, gini)
  }
)


#' @rdname metrics
#' 
mae <- function(observed, predicted = NULL, ...) {
  .mae(observed, predicted)
}

MLMetric(mae) <- list("mae", "Mean Absolute Error", FALSE)


setGeneric(".mae",
           function(observed, predicted, ...) standardGeneric(".mae"))


setMethod(".mae", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".mae", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    .metric.matrix(observed, predicted, mae)
  }
)


setMethod(".mae", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    mean(abs(observed - predicted))
  }
)


setMethod(".mae", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.SurvMean(observed, predicted, mae)
  }
)


#' @rdname metrics
#' 
mse <- function(observed, predicted = NULL, ...) {
  .mse(observed, predicted)
}

MLMetric(mse) <- list("mse", "Mean Squared Error", FALSE)


setGeneric(".mse",
           function(observed, predicted, ...) standardGeneric(".mse"))


setMethod(".mse", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".mse", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    .metric.matrix(observed, predicted, mse)
  }
)


setMethod(".mse", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    mean((observed - predicted)^2)
  }
)


setMethod(".mse", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.SurvMean(observed, predicted, mse)
  }
)


#' @rdname metrics
#' 
msle <- function(observed, predicted = NULL, ...) {
  .msle(observed, predicted)
}

MLMetric(msle) <- list("msle", "Mean Squared Log Error", FALSE)


setGeneric(".msle",
           function(observed, predicted, ...) standardGeneric(".msle"))


setMethod(".msle", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".msle", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    .metric.matrix(observed, predicted, msle)
  }
)


setMethod(".msle", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    mean((log(1 + observed) - log(1 + predicted))^2)
  }
)


setMethod(".msle", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.SurvMean(observed, predicted, msle)
  }
)


#' @rdname metrics
#' 
r2 <- function(observed, predicted = NULL, ...) {
  .r2(observed, predicted)
}

MLMetric(r2) <- list("r2", "Coefficient of Determination", TRUE)


setGeneric(".r2",
           function(observed, predicted, ...) standardGeneric(".r2"))


setMethod(".r2", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".r2", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    .metric.matrix(observed, predicted, r2)
  }
)


setMethod(".r2", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
  }
)


setMethod(".r2", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.SurvMean(observed, predicted, r2)
  }
)


#' @rdname metrics
#' 
rmse <- function(observed, predicted = NULL, ...) {
  .rmse(observed, predicted)
}

MLMetric(rmse) <- list("rmse", "Root Mean Squared Error", FALSE)


setGeneric(".rmse",
           function(observed, predicted, ...) standardGeneric(".rmse"))


setMethod(".rmse", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".rmse", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    sqrt(mse(observed, predicted))
  }
)


setMethod(".rmse", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    sqrt(mse(observed, predicted))
  }
)


setMethod(".rmse", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    sqrt(mse(observed, predicted))
  }
)


#' @rdname metrics
#' 
rmsle <- function(observed, predicted = NULL, ...) {
  .rmsle(observed, predicted)
}

MLMetric(rmsle) <- list("rmsle", "Root Mean Squared Log Error", FALSE)


setGeneric(".rmsle",
           function(observed, predicted, ...) standardGeneric(".rmsle"))


setMethod(".rmsle", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".rmsle", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    sqrt(msle(observed, predicted))
  }
)


setMethod(".rmsle", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    sqrt(msle(observed, predicted))
  }
)


setMethod(".rmsle", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    sqrt(msle(observed, predicted))
  }
)


.metric.matrix <- function(observed, predicted, FUN, ...) {
  mean(sapply(1:ncol(observed), function(i) {
    FUN(observed[, i], predicted[, i], ...)
  }))
}


.metric.SurvMean <- function(observed, predicted, FUN, ...) {
  events <- observed[, "status"] == 1
  FUN(observed[events, "time"], predicted[events], ...)
}
