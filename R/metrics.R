#' Performance Metrics
#' 
#' Compute measures of agreement between observed and predicted responses.
#' 
#' @aliases metrics
#' @rdname metrics
#' 
#' @param observed observed responses.
#' @param predicted predicted responses.
#' @param cutoff threshold above which probabilities are classified as success
#' for binary responses.
#' @param cutoff_index function to calculate a desired sensitivity-specificity
#' tradeoff.
#' @param times numeric vector of follow-up times at which survival events
#' were predicted.
#' @param ... arguments passed to or from other methods.
#' 
#' @seealso \code{\link{modelmetrics}}
#' 
accuracy <- function(observed, predicted, cutoff = 0.5, ...) {
  .accuracy(observed, predicted, cutoff = cutoff)
}


setGeneric(".accuracy",
           function(observed, predicted, ...) standardGeneric(".accuracy"))


setMethod(".accuracy", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".accuracy", c("factor", "factor"),
  function(observed, predicted, ...) {
    mean(observed == predicted)
  }
)


setMethod(".accuracy", c("factor", "matrix"),
  function(observed, predicted, ...) {
    predicted <- convert_response(observed, predicted)
    accuracy(observed, predicted)
  }
)


setMethod(".accuracy", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    predicted <- convert_response(observed, predicted, cutoff = cutoff)
    accuracy(observed, predicted)
  }
)


#' @rdname metrics
#' 
brier <- function(observed, predicted, times = numeric(), ...) {
  .brier(observed, predicted, times = times)
}


setGeneric(".brier",
           function(observed, predicted, ...) standardGeneric(".brier"))


setMethod(".brier", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".brier", c("factor", "matrix"),
  function(observed, predicted, ...) {
    observed <- model.matrix(~ observed - 1)
    sum((observed - predicted)^2) / nrow(observed)
  }
)


setMethod(".brier", c("factor", "numeric"),
  function(observed, predicted, ...) {
    mse(as.numeric(observed == levels(observed)[2]), predicted)
  }
)


setMethod(".brier", c("Surv", "matrix"),
  function(observed, predicted, times = numeric(), ...) {
    stopifnot(ncol(predicted) == length(times))
    
    obs_times <- observed[, "time"]
    obs_events <- observed[, "status"]
    fitcens <- survfit(Surv(obs_times, 1 - obs_events) ~ 1)
  
    metrics <- sapply(seq(times), function(i) {
      time <- times[i]
      is_obs_after <- obs_times > time
      weights <- (obs_events == 1 | is_obs_after) /
        predict(fitcens, pmin(obs_times, time))
      mean(weights * (is_obs_after - predicted[, i])^2)
    })
    
    if (length(times) > 1) {
      c("mean" = mean.SurvMetrics(metrics, times), "time" = metrics)
    } else {
      metrics
    }
  }
)


#' @rdname metrics
#' 
cindex <- function(observed, predicted, ...) {
  .cindex(observed, predicted)
}


setGeneric(".cindex",
           function(observed, predicted, ...) standardGeneric(".cindex"))


setMethod(".cindex", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".cindex", c("factor", "numeric"),
  function(observed, predicted, ...) {
    roc_auc(observed, predicted)
  }
)


setMethod(".cindex", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    rcorr.cens(-predicted, observed)[["C Index"]]
  }
)


#' @rdname metrics
#' 
cross_entropy <- function(observed, predicted, ...) {
  .cross_entropy(observed, predicted)
}


setGeneric(".cross_entropy",
           function(observed, predicted, ...) standardGeneric(".cross_entropy"))


setMethod(".cross_entropy", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".cross_entropy", c("factor", "matrix"),
  function(observed, predicted, ...) {
    observed <- model.matrix(~ observed - 1)
    eps <- 1e-15
    predicted <- pmax(pmin(predicted, 1 - eps), eps)
    -sum(observed * log(predicted)) / nrow(predicted)
  }
)

setMethod(".cross_entropy", c("factor", "numeric"),
  function(observed, predicted, ...) {
    cross_entropy(observed, cbind(1 - predicted, predicted))
  }
)


#' @rdname metrics
#' 
#' @param beta relative importance of recall to precision in the calculation of
#' \code{f_score} [default: F1 score].
#' 
f_score <- function(observed, predicted, cutoff = 0.5, beta = 1, ...) {
  .f_score(observed, predicted, cutoff = cutoff, beta = beta)
}


setGeneric(".f_score",
           function(observed, predicted, ...) standardGeneric(".f_score"))


setMethod(".f_score", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".f_score", c("factor", "numeric"),
  function(observed, predicted, cutoff, beta, ...) {
    n <- confusion(observed, predicted, cutoff = cutoff)
    beta2 <- beta^2
    (1 + beta2) * n[2, 2] / ((1 + beta2) * n[2, 2] + beta2 * n[1, 2] + n[2, 1])
  }
)


#' @rdname metrics
#' 
kappa <- function(observed, predicted, cutoff = 0.5, ...) {
  .kappa(observed, predicted, cutoff = cutoff)
}


setGeneric(".kappa",
           function(observed, predicted, ...) standardGeneric(".kappa"))


setMethod(".kappa", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".kappa", c("factor", "factor"),
  function(observed, predicted, ...) {
    p <- prop.table(confusion(observed, predicted))
    1 - (1 - sum(diag(p))) / (1 - sum(rowSums(p) * colSums(p)))
  }
)


setMethod(".kappa", c("factor", "matrix"),
  function(observed, predicted, ...) {
    predicted <- convert_response(observed, predicted)
    kappa(observed, predicted)
  }
)


setMethod(".kappa", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    predicted <- convert_response(observed, predicted, cutoff = cutoff)
    kappa(observed, predicted)
  }
)


#' @rdname metrics
#' 
mae <- function(observed, predicted, ...) {
  .mae(observed, predicted)
}

setGeneric(".mae",
           function(observed, predicted, ...) standardGeneric(".mae"))


setMethod(".mae", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".mae", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    mean(abs(observed - predicted))
  }
)


setMethod(".mae", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    n <- ncol(observed)
    sapply(1:n, function(i) mae(observed[, i], predicted[, i])) / n
  }
)


mean.SurvMetrics <- function(x, times) {
  weights <- diff(c(0, times)) / tail(times, 1)
  sum(weights * x)
}


#' @rdname metrics
#' 
mse <- function(observed, predicted, ...) {
  .mse(observed, predicted)
}


setGeneric(".mse",
           function(observed, predicted, ...) standardGeneric(".mse"))


setMethod(".mse", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".mse", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    mean((observed - predicted)^2)
  }
)


setMethod(".mse", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    n <- ncol(observed)
    sapply(1:n, function(i) mse(observed[, i], predicted[, i])) / n
  }
)


#' @rdname metrics
#' 
npv <- function(observed, predicted, cutoff = 0.5, ...) {
  .npv(observed, predicted, cutoff = cutoff)
}


setGeneric(".npv",
           function(observed, predicted, ...) standardGeneric(".npv"))


setMethod(".npv", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".npv", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    n <- confusion(observed, predicted, cutoff = cutoff)
    n[1, 1] / (n[1, 1] + n[1, 2])
  }
)


#' @rdname metrics
#' 
ppv <- function(observed, predicted, cutoff = 0.5, ...) {
  .ppv(observed, predicted, cutoff = cutoff)
}


setGeneric(".ppv",
           function(observed, predicted, ...) standardGeneric(".ppv"))


setMethod(".ppv", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".ppv", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    n <- confusion(observed, predicted, cutoff = cutoff)
    n[2, 2] / (n[2, 1] + n[2, 2])
  }
)


#' @rdname metrics
#' 
pr_auc <- function(observed, predicted, ...) {
  .pr_auc(observed, predicted)
}

setGeneric(".pr_auc",
           function(observed, predicted, ...) standardGeneric(".pr_auc"))


setMethod(".pr_auc", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".pr_auc", c("factor", "numeric"),
  function(observed, predicted, ...) {
    PRAUC(predicted, observed == levels(observed)[2])
  }
)


#' @rdname metrics
#' 
precision <- function(observed, predicted, cutoff = 0.5, ...) {
  .precision(observed, predicted, cutoff = cutoff)
}


setGeneric(".precision",
           function(observed, predicted, ...) standardGeneric(".precision"))


setMethod(".precision", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".precision", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    ppv(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
r2 <- function(observed, predicted, ...) {
  .r2(observed, predicted)
}


setGeneric(".r2",
           function(observed, predicted, ...) standardGeneric(".r2"))


setMethod(".r2", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".r2", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
  }
)


setMethod(".r2", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    n <- ncol(observed)
    sapply(1:n, function(i) r2(observed[, i], predicted[, i])) / n
  }
)


#' @rdname metrics
#' 
recall <- function(observed, predicted, cutoff = 0.5, ...) {
  .recall(observed, predicted, cutoff = cutoff)
}


setGeneric(".recall",
           function(observed, predicted, ...) standardGeneric(".recall"))


setMethod(".recall", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".recall", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    sensitivity(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
rmse <- function(observed, predicted, ...) {
  .rmse(observed, predicted)
}


setGeneric(".rmse",
           function(observed, predicted, ...) standardGeneric(".rmse"))


setMethod(".rmse", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".rmse", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    sqrt(mse(observed, predicted))
  }
)


#' @rdname metrics
#' 
roc_auc <- function(observed, predicted, times = numeric(), ...) {
  .roc_auc(observed, predicted, times = times)
}


setGeneric(".roc_auc",
           function(observed, predicted, ...) standardGeneric(".roc_auc"))


setMethod(".roc_auc", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".roc_auc", c("factor", "numeric"),
  function(observed, predicted, ...) {
    R <- rank(predicted)
    is_event <- observed == levels(observed)[2]
    n_event <- sum(is_event)
    n_nonevent <- length(observed) - n_event
    (sum(R[is_event]) - n_event * (n_event + 1) / 2) / (n_event * n_nonevent)
  }
)


setMethod(".roc_auc", c("Surv", "matrix"),
  function(observed, predicted, times, ...) {
    stopifnot(ncol(predicted) == length(times))
    
    metrics <- sapply(seq(times), function(i) {
      survivalROC(observed[, "time"], observed[, "status"], 1 - predicted[, i],
                  predict.time = times[i], method = "KM")$AUC
    })
    
    if (length(times) > 1) {
      c("mean" = mean.SurvMetrics(metrics, times), "time" = metrics)
    } else {
      metrics
    }
  }
)


#' @rdname metrics
#' 
roc_index <- function(observed, predicted, cutoff = 0.5, cutoff_index =
                        function(sens, spec) sens + spec, ...) {
  .roc_index(observed, predicted, cutoff = cutoff, cutoff_index = cutoff_index)
}


setGeneric(".roc_index",
           function(observed, predicted, ...) standardGeneric(".roc_index"))


setMethod(".roc_index", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".roc_index", c("factor", "numeric"),
  function(observed, predicted, cutoff, cutoff_index, ...) {
    sens <- sensitivity(observed, predicted, cutoff = cutoff)
    spec <- specificity(observed, predicted, cutoff = cutoff)
    cutoff_index(sens, spec)
  }
)


#' @rdname metrics
#' 
sensitivity <- function(observed, predicted, cutoff = 0.5, ...) {
  .sensitivity(observed, predicted, cutoff = cutoff)
}


setGeneric(".sensitivity",
           function(observed, predicted, ...) standardGeneric(".sensitivity"))


setMethod(".sensitivity", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".sensitivity", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    n <- confusion(observed, predicted, cutoff = cutoff)
    n[2, 2] / (n[1, 2] + n[2, 2])
  }
)


#' @rdname metrics
#' 
specificity <- function(observed, predicted, cutoff = 0.5, ...) {
  .specificity(observed, predicted, cutoff = cutoff)
}


setGeneric(".specificity",
           function(observed, predicted, ...) standardGeneric(".specificity"))


setMethod(".specificity", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".specificity", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    n <- confusion(observed, predicted, cutoff = cutoff)
    n[1, 1] / (n[1, 1] + n[2, 1])
  }
)


#' @rdname metrics
#' 
#' @param weight definition of weights for the kappa metric as linear
#' (\code{"equal"}) or squared (\code{"squared"}) distance between ordinal
#' categories.
#' 
weighted_kappa <- function(observed, predicted, weight = c("equal", "squared"),
                           ...) {
  .weighted_kappa(observed, predicted, weight = match.arg(weight))
}


setGeneric(".weighted_kappa",
           function(observed, predicted, ...) standardGeneric(".weighted_kappa"))


setMethod(".weighted_kappa", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".weighted_kappa", c("ordered", "factor"),
  function(observed, predicted, weight, ...) {
    ratings <- cbind(observed, predicted)
    kappa2(ratings, weight = weight)$value
  }
)


setMethod(".weighted_kappa", c("ordered", "matrix"),
  function(observed, predicted, weight, ...) {
    predicted <- convert_response(observed, predicted)
    weighted_kappa(observed, predicted, weight = weight)
  }
)
