#' Performance Metrics
#' 
#' Compute measures of agreement between observed and predicted responses.
#' 
#' @name metrics
#' @rdname metrics
#' 
#' @param observed observed responses or
#' \code{\link[=confusion]{ConfusionMatrix}} of observed and predicted
#' responses.
#' @param predicted predicted responses.
#' @param beta relative importance of recall to precision in the calculation of
#' \code{f_score} [default: F1 score].
#' @param cutoff threshold above which binary factor probabilities are
#' classified as events and below which survival probabilities are classified.
#' @param f function to calculate a desired sensitivity-specificity tradeoff.
#' @param power power to which positional distances of off-diagonals from the
#' main diagonal in confusion matrices are raised to calculate
#' \code{weighted_kappa2}.
#' @param times numeric vector of follow-up times at which survival
#' probabilities were predicted.
#' @param ... arguments passed to or from other methods.
#' 
#' @seealso \code{\link{metricinfo}}, \code{\link{confusion}},
#' \code{\link{performance}}
#' 
accuracy <- function(observed, predicted = NULL, cutoff = 0.5,
                     times = numeric(), ...) {
  .accuracy(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(accuracy) <- list("accuracy", "Accuracy", TRUE)


setGeneric(".accuracy",
           function(observed, predicted, ...) standardGeneric(".accuracy"))


setMethod(".accuracy", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".accuracy", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    sum(diag(observed)) / sum(observed)
  }
)


setMethod(".accuracy", c("factor", "factor"),
  function(observed, predicted, ...) {
    accuracy(confusion(observed, predicted))
  }
)


setMethod(".accuracy", c("factor", "matrix"),
  function(observed, predicted, ...) {
    accuracy(confusion(observed, predicted))
  }
)


setMethod(".accuracy", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    accuracy(confusion(observed, predicted, cutoff = cutoff))
  }
)


setMethod(".accuracy", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, accuracy)
  }
)


#' @rdname metrics
#' 
brier <- function(observed, predicted = NULL, times = numeric(), ...) {
  .brier(observed, predicted, times = times)
}

MLMetric(brier) <- list("brier", "Brier Score", FALSE)


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
    if (length(times) != ncol(predicted)) {
      stop("unequal number of survival times and predictions")
    }
    
    obs_times <- observed[, "time"]
    obs_events <- observed[, "status"]
    fitcens <- survfit(Surv(obs_times, 1 - obs_events) ~ 1)
  
    metrics <- sapply(seq_along(times), function(i) {
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
cindex <- function(observed, predicted = NULL, ...) {
  .cindex(observed, predicted)
}

MLMetric(cindex) <- list("cindex", "Concordance Index", TRUE)


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
    Hmisc::rcorr.cens(predicted, observed)[["C Index"]]
  }
)


#' @rdname metrics
#' 
cross_entropy <- function(observed, predicted = NULL, ...) {
  .cross_entropy(observed, predicted)
}

MLMetric(cross_entropy) <- list("cross_entropy", "Cross Entropy", FALSE)


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
f_score <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                    beta = 1, ...) {
  .f_score(observed, predicted, cutoff = cutoff, times = times, beta = beta)
}

MLMetric(f_score) <- list("f_score", "F Score", TRUE)


setGeneric(".f_score",
           function(observed, predicted, ...) standardGeneric(".f_score"))


setMethod(".f_score", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".f_score", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, beta, ...) {
    if (any(dim(observed) != c(2, 2))) {
      warn("'f_score' requires a 2-level response")
      numeric()
    } else {
      beta2 <- beta^2
      (1 + beta2) * observed[2, 2] /
        ((1 + beta2) * observed[2, 2] + beta2 * observed[1, 2] + observed[2, 1])
    }
  }
)


setMethod(".f_score", c("factor", "numeric"),
  function(observed, predicted, cutoff, beta, ...) {
    f_score(confusion(observed, predicted, cutoff = cutoff), beta = beta)
  }
)


setMethod(".f_score", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, beta, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, f_score,
                        beta = beta)
  }
)


#' @rdname metrics
#' 
fnr <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                ...) {
  .fnr(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(fnr) <- list("fnr", "False Negative Rate", FALSE)


setGeneric(".fnr", function(observed, predicted, ...) standardGeneric(".fnr"))


setMethod(".fnr", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".fnr", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    1 - tpr(observed)
  }
)


setMethod(".fnr", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    1 - tpr(observed, predicted, cutoff = cutoff)
  }
)


setMethod(".fnr", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    1 - tpr(observed, predicted, cutoff = cutoff, times = times)
  }
)


#' @rdname metrics
#' 
fpr <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                ...) {
  .fpr(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(fpr) <- list("fpr", "False Positive Rate", FALSE)


setGeneric(".fpr", function(observed, predicted, ...) standardGeneric(".fpr"))


setMethod(".fpr", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".fpr", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    1 - tnr(observed)
  }
)


setMethod(".fpr", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    1 - tnr(observed, predicted, cutoff = cutoff)
  }
)


setMethod(".fpr", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    1 - tnr(observed, predicted, cutoff = cutoff, times = times)
  }
)


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
    y_predicted <- observed[order(predicted, decreasing = TRUE)]
    y_observed <- observed[order(observed, decreasing = TRUE)]
    gini_sum <- function(y) sum(cumsum(y / sum(y) - 1 / length(y)))
    gini_sum(y_predicted) / gini_sum(y_observed)
  }
)


setMethod(".gini", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.Surv_numeric(observed, predicted, gini)
  }
)


#' @rdname metrics
#' 
kappa2 <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                   ...) {
  .kappa2(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(kappa2) <- list("kappa2", "Cohen's Kappa", TRUE)


setGeneric(".kappa2",
           function(observed, predicted, ...) standardGeneric(".kappa2"))


setMethod(".kappa2", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".kappa2", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    p <- prop.table(observed)
    1 - (1 - sum(diag(p))) / (1 - sum(rowSums(p) * colSums(p)))
  }
)


setMethod(".kappa2", c("factor", "factor"),
  function(observed, predicted, ...) {
    kappa2(confusion(observed, predicted))
  }
)


setMethod(".kappa2", c("factor", "matrix"),
  function(observed, predicted, ...) {
    kappa2(confusion(observed, predicted))
  }
)


setMethod(".kappa2", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    kappa2(confusion(observed, predicted, cutoff = cutoff))
  }
)


setMethod(".kappa2", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, kappa2)
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
    .metric.Surv_numeric(observed, predicted, mae)
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
    .metric.Surv_numeric(observed, predicted, mse)
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
    .metric.Surv_numeric(observed, predicted, msle)
  }
)


#' @rdname metrics
#' 
npv <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                ...) {
  .npv(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(npv) <- list("npv", "Negative Predictive Value", TRUE)


setGeneric(".npv",
           function(observed, predicted, ...) standardGeneric(".npv"))


setMethod(".npv", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".npv", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    if (any(dim(observed) != c(2, 2))) {
      warn("'npv' requires a 2-level response")
      numeric()
    } else {
      observed[1, 1] / (observed[1, 1] + observed[1, 2])
    }
  }
)


setMethod(".npv", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    npv(confusion(observed, predicted, cutoff = cutoff))
  }
)


setMethod(".npv", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, npv)
  }
)


#' @rdname metrics
#' 
ppv <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                ...) {
  .ppv(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(ppv) <- list("ppv", "Positive Predictive Value", TRUE)


setGeneric(".ppv",
           function(observed, predicted, ...) standardGeneric(".ppv"))


setMethod(".ppv", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".ppv", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    if (any(dim(observed) != c(2, 2))) {
      warn("'ppv' requires a 2-level response")
      numeric()
    } else {
      observed[2, 2] / (observed[2, 1] + observed[2, 2])
    }
  }
)


setMethod(".ppv", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    ppv(confusion(observed, predicted, cutoff = cutoff))
  }
)


setMethod(".ppv", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, ppv)
  }
)


#' @rdname metrics
#' 
pr_auc <- function(observed, predicted = NULL, times = numeric(), ...) {
  .pr_auc(observed, predicted, times = times)
}

MLMetric(pr_auc) <- list("pr_auc", "Area Under Precision-Recall Curve", TRUE)


setGeneric(".pr_auc",
           function(observed, predicted, ...) standardGeneric(".pr_auc"))


setMethod(".pr_auc", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".pr_auc", c("factor", "matrix"), 
  function(observed, predicted, ...) {
    .metric.factor(observed, predicted, pr_auc)
  }
)


setMethod(".pr_auc", c("factor", "numeric"),
  function(observed, predicted, ...) {
    cutoffs <- c(sort(unique(predicted), decreasing = TRUE)[-1], -Inf)
    num_cutoffs <- length(cutoffs)
    if (num_cutoffs <= 1) NA else {
      perf <- data.frame(x = numeric(num_cutoffs), y = numeric(num_cutoffs))
      for (i in 1:num_cutoffs) {
        conf <- confusion(observed, predicted, cutoff = cutoffs[i])
        perf$x[i] <- recall(conf)
        perf$y[i] <- precision(conf)
      }
      with(perf, sum(diff(x) * (y[-num_cutoffs] + y[-1]) / 2))
    }
  }
)


setMethod(".pr_auc", c("Surv", "matrix"),
  function(observed, predicted, times, ...) {
    .auc.Surv(observed, predicted, times, recall, precision)
  }
)


#' @rdname metrics
#' 
precision <- function(observed, predicted = NULL, cutoff = 0.5,
                      times = numeric(), ...) {
  .precision(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(precision) <- list("precision", "Precision", TRUE)


setGeneric(".precision",
           function(observed, predicted, ...) standardGeneric(".precision"))


setMethod(".precision", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".precision", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    ppv(observed)
  }
)


setMethod(".precision", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    ppv(observed, predicted, cutoff = cutoff)
  }
)


setMethod(".precision", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    ppv(observed, predicted, cutoff = cutoff, times = times)
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
    .metric.Surv_numeric(observed, predicted, r2)
  }
)


#' @rdname metrics
#' 
recall <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                   ...) {
  .recall(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(recall) <- list("recall", "Recall", TRUE)


setGeneric(".recall",
           function(observed, predicted, ...) standardGeneric(".recall"))


setMethod(".recall", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".recall", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    tpr(observed)
  }
)


setMethod(".recall", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    tpr(observed, predicted, cutoff = cutoff)
  }
)


setMethod(".recall", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    tpr(observed, predicted, cutoff = cutoff, times = times)
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
    .metric.matrix(observed, predicted, rmse)
  }
)


setMethod(".rmse", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    sqrt(mse(observed, predicted))
  }
)


setMethod(".rmse", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.Surv_numeric(observed, predicted, rmse)
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
    .metric.matrix(observed, predicted, rmsle)
  }
)


setMethod(".rmsle", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    sqrt(msle(observed, predicted))
  }
)


setMethod(".rmsle", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    .metric.Surv_numeric(observed, predicted, rmsle)
  }
)


#' @rdname metrics
#' 
roc_auc <- function(observed, predicted = NULL, times = numeric(), ...) {
  .roc_auc(observed, predicted, times = times)
}

MLMetric(roc_auc) <- list("roc_auc", "Area Under ROC Curve", TRUE)


setGeneric(".roc_auc",
           function(observed, predicted, ...) standardGeneric(".roc_auc"))


setMethod(".roc_auc", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".roc_auc", c("factor", "matrix"), 
  function(observed, predicted, ...) {
    .metric.factor(observed, predicted, roc_auc)
  }
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
    .auc.Surv(observed, predicted, times, fpr, tpr)
  }
)


#' @rdname metrics
#' 
roc_index <- function(observed, predicted = NULL, cutoff = 0.5,
                      times = numeric(), f = function(sens, spec) sens + spec,
                      ...) {
  .roc_index(observed, predicted, cutoff = cutoff, times = times, f = f)
}

MLMetric(roc_index) <- list("roc_index", "ROC Index", TRUE)


setGeneric(".roc_index",
           function(observed, predicted, ...) standardGeneric(".roc_index"))


setMethod(".roc_index", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".roc_index", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, f, ...) {
    f(sensitivity(observed), specificity(observed))
  }
)


setMethod(".roc_index", c("factor", "numeric"),
  function(observed, predicted, cutoff, f, ...) {
    roc_index(confusion(observed, predicted, cutoff = cutoff), f = f)
  }
)


setMethod(".roc_index", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, f, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, roc_index, f = f)
  }
)


#' @rdname metrics
#' 
sensitivity <- function(observed, predicted = NULL, cutoff = 0.5,
                        times = numeric(), ...) {
  .sensitivity(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(sensitivity) <- list("sensitivity", "Sensitivity", TRUE)


setGeneric(".sensitivity",
           function(observed, predicted, ...) standardGeneric(".sensitivity"))


setMethod(".sensitivity", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".sensitivity", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    tpr(observed)
  }
)


setMethod(".sensitivity", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    tpr(observed, predicted, cutoff = cutoff)
  }
)


setMethod(".sensitivity", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    tpr(observed, predicted, cutoff = cutoff, times = times)
  }
)


#' @rdname metrics
#' 
specificity <- function(observed, predicted = NULL, cutoff = 0.5,
                        times = numeric(), ...) {
  .specificity(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(specificity) <- list("specificity", "Specificity", TRUE)


setGeneric(".specificity",
           function(observed, predicted, ...) standardGeneric(".specificity"))


setMethod(".specificity", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".specificity", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    tnr(observed)
  }
)


setMethod(".specificity", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    tnr(observed, predicted, cutoff = cutoff)
  }
)


setMethod(".specificity", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    tnr(observed, predicted, cutoff = cutoff, times = times)
  }
)


#' @rdname metrics
#' 
tnr <- function(observed, predicted = NULL, cutoff = 0.5, times = numeric(),
                ...) {
  .tnr(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(tnr) <- list("tnr", "True Negative Rate", TRUE)


setGeneric(".tnr", function(observed, predicted, ...) standardGeneric(".tnr"))


setMethod(".tnr", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".tnr", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    if (any(dim(observed) != c(2, 2))) {
      warn("'tnr' requires a 2-level response")
      numeric()
    } else{
      observed[1, 1] / (observed[1, 1] + observed[2, 1])
    }
  }
)


setMethod(".tnr", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    tnr(confusion(observed, predicted, cutoff = cutoff))
  }
)


setMethod(".tnr", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, tnr)
  }
)


#' @rdname metrics
#' 
tpr <- function(observed, predicted = NULL, cutoff = 0.5,
                times = numeric(), ...) {
  .tpr(observed, predicted, cutoff = cutoff, times = times)
}

MLMetric(tpr) <- list("tpr", "True Positive Rate", TRUE)


setGeneric(".tpr", function(observed, predicted, ...) standardGeneric(".tpr"))


setMethod(".tpr", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".tpr", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    if (any(dim(observed) != c(2, 2))) {
      warn("'tpr' requires a 2-level response")
      numeric()
    } else {
      observed[2, 2] / (observed[1, 2] + observed[2, 2])
    }
  }
)


setMethod(".tpr", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    tpr(confusion(observed, predicted, cutoff = cutoff))
  }
)


setMethod(".tpr", c("Surv", "matrix"),
  function(observed, predicted, cutoff, times, ...) {
    .metric.Surv_matrix(observed, predicted, cutoff, times, tpr)
  }
)


#' @rdname metrics
#' 
weighted_kappa2 <- function(observed, predicted = NULL, power = 1, ...) {
  .weighted_kappa2(observed, predicted, power = power)
}

MLMetric(weighted_kappa2) <- list("weighted_kappa2", "Weighted Cohen's Kappa",
                                  TRUE)


setGeneric(".weighted_kappa2", function(observed, predicted, ...)
  standardGeneric(".weighted_kappa2"))


setMethod(".weighted_kappa2", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".weighted_kappa2", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, power, ...) {
    expected <- (rowSums(observed) %o% colSums(observed)) / sum(observed)
    weights <- abs(row(observed) - col(observed))^power
    1 - sum(weights * observed) / sum(weights * expected)
  }
)


setMethod(".weighted_kappa2", c("ordered", "ordered"),
  function(observed, predicted, power, ...) {
    weighted_kappa2(confusion(observed, predicted), power = power)
  }
)


setMethod(".weighted_kappa2", c("ordered", "matrix"),
  function(observed, predicted, power, ...) {
    weighted_kappa2(confusion(observed, predicted), power = power)
  }
)


.auc.Surv <-   function(observed, predicted, times, FUN_x, FUN_y) {
  if (length(times) != ncol(predicted)) {
    stop("unequal number of survival times and predictions")
  }
  
  surv_all <- predict(survfit(observed ~ 1, se.fit = FALSE), times)

  metrics <- sapply(1:ncol(predicted), function(i) {
    pred <- predicted[, i]
    cutoffs <- c(-Inf, sort(unique(pred)))
    time <- times[i]

    conf <- ConfusionMatrix(table(Predicted = 0:1, Observed = 0:1))
    num_cutoffs <- length(cutoffs)
    perf <- data.frame(x = numeric(num_cutoffs), y = numeric(num_cutoffs))
    for (j in 1:num_cutoffs) {
      surv_pos <- 1
      positives <- pred <= cutoffs[j]
      p <- mean(positives)
      if (p > 0) {
        obs <- observed[positives]
        valid_events <- obs[, "status"] == 1 & obs[, "time"] <= time
        event_times <- sort(unique(obs[valid_events, "time"]))
        for (event_time in event_times) {
          d <- sum(obs[, "time"] == event_time & obs[, "status"] == 1)
          n <- sum(obs[, "time"] >= event_time)
          surv_pos <- surv_pos * (1 - d / n)
        }
      }
      conf[1, 1] <- surv_all[i] - surv_pos * p
      conf[1, 2] <- (1 - p) - conf[1, 1]
      conf[2, 2] <- (1 - surv_pos) * p
      conf[2, 1] <- p - conf[2, 2]
      perf$x[j] <- FUN_x(conf)
      perf$y[j] <- FUN_y(conf)
    }
    perf <- na.omit(perf)
    n <- nrow(perf)
    if (n > 1) with(perf, sum(diff(x) * (y[-n] + y[-1]) / 2)) else NA
  })

  if (length(times) > 1) {
    c("mean" = mean.SurvMetrics(metrics, times), "time" = metrics)
  } else {
    metrics
  }
}


.metric.factor <- function(observed, predicted, FUN, ...) {
  mean(sapply(1:ncol(predicted), function(i) {
    FUN(factor(observed == levels(observed)[i]), predicted[, i], ...)
  }))
}


.metric.matrix <- function(observed, predicted, FUN, ...) {
  mean(sapply(1:ncol(observed), function(i) {
    FUN(observed[, i], predicted[, i], ...)
  }))
}


.metric.Surv_matrix <- function(observed, predicted, cutoff, times, FUN, ...) {
  conf <- confusion(observed, predicted, cutoff = cutoff, times = times)
  metrics <- sapply(conf, FUN, ...)
  if (length(times) > 1) {
    c("mean" = mean.SurvMetrics(metrics, times), metrics)
  } else {
    metrics[[1]]
  }
}


.metric.Surv_numeric <- function(observed, predicted, FUN, ...) {
  events <- observed[, "status"] == 1
  FUN(observed[events, "time"], predicted[events], ...)
}


mean.SurvMetrics <- function(x, times) {
  weights <- diff(c(0, times)) / tail(times, 1)
  sum(weights * x)
}
