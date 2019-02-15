#' Performance Metrics
#' 
#' Compute measures of agreement between observed and predicted responses.
#' 
#' @name metrics
#' @rdname metrics
#' 
#' @param observed observed responses, \code{\link{Curves}} object, or
#' \code{\link[=confusion]{ConfusionMatrix}} of observed and predicted
#' responses.
#' @param predicted predicted responses.
#' @param beta relative importance of recall to precision in the calculation of
#' \code{f_score} [default: F1 score].
#' @param cutoff threshold above which binary factor probabilities are
#' classified as events and below which survival probabilities are classified.
#' @param f function to calculate a desired sensitivity-specificity tradeoff.
#' @param metrics list of two performance metrics for the calculation [default:
#' ROC metrics].
#' @param power power to which positional distances of off-diagonals from the
#' main diagonal in confusion matrices are raised to calculate
#' \code{weighted_kappa2}.
#' @param stat function to compute a summary statistic at each cutoff value of
#' resampled metrics in \code{Curves}, or \code{NULL} for resample-specific
#' metrics.
#' @param ... arguments passed to or from other methods.
#' 
#' @seealso \code{\link{metricinfo}}, \code{\link{confusion}},
#' \code{\link{performance}}, \code{\link{performance_curve}}
#' 
accuracy <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .accuracy(observed, predicted, cutoff = cutoff)
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


setMethod(".accuracy", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    .metric.Surv_matrix(observed, predicted, accuracy)
  }
)


setMethod(".accuracy", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    .metric.Surv_matrix(observed, predicted, accuracy, cutoff)
  }
)


#' @rdname metrics
#' 
auc <- function(observed, predicted = NULL,
                metrics = c(MachineShop::tpr, MachineShop::fpr),
                stat = base::mean, ...) {
  .auc(observed, predicted, metrics = metrics, stat = stat)
}

MLMetric(auc) <- list("auc", "Area Under Performance Curve", TRUE)


setGeneric(".auc", function(observed, predicted, ...) standardGeneric(".auc"))


setMethod(".auc", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".auc", c("factor", "numeric"),
  function(observed, predicted, metrics, ...) {
    if (identical(metrics[[1]], tpr) && identical(metrics[[2]], fpr)) {
      R <- rank(predicted)
      is_event <- observed == levels(observed)[2]
      n_event <- sum(is_event)
      n_nonevent <- length(observed) - n_event
      (sum(R[is_event]) - n_event * (n_event + 1) / 2) / (n_event * n_nonevent)
    } else {
      unname(auc(performance_curve(observed, predicted, metrics = metrics)))
    }
  }
)


setMethod(".auc", c("Curves", "NULL"),
  function(observed, predicted, stat, ...) {
    observed <- summary(observed, stat = stat)
    indices <- observed["Model"]
    indices$Resample <- observed$Resample
    by(observed, indices, function(split) {
      split <- na.omit(split)
      n <- nrow(split)
      if (n > 1) with(split, sum(diff(x) * (y[-n] + y[-1]) / 2)) else NA
    })  
  }
)


setMethod(".auc", c("Surv", "SurvProbs"),
  function(observed, predicted, metrics, ...) {
    x <- unname(auc(performance_curve(observed, predicted, metrics = metrics)))
    times <- predicted@times
    if (length(times) > 1) {
      c("mean" = mean.SurvMetrics(x, times), "time" = x)
    } else {
      x
    }
  }
)


#' @rdname metrics
#' 
brier <- function(observed, predicted = NULL, ...) {
  .brier(observed, predicted)
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


setMethod(".brier", c("Surv", "SurvProbs"),
  function(observed, predicted, ...) {
    times <- predicted@times
    obs_times <- observed[, "time"]
    obs_events <- observed[, "status"]
    
    cens_fit <- survfit(Surv(obs_times, 1 - obs_events) ~ 1, se.fit = FALSE)
  
    metrics <- sapply(seq_along(times), function(i) {
      time <- times[i]
      obs_after_time <- obs_times > time
      cens <- predict(cens_fit, pmin(obs_times, time))
      weights <- ifelse(obs_events == 1 | obs_after_time, 1 / cens, 0)
      mean(weights * (obs_after_time - predicted[, i])^2)
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
f_score <- function(observed, predicted = NULL, cutoff = 0.5, beta = 1, ...) {
  .f_score(observed, predicted, cutoff = cutoff, beta = beta)
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


setMethod(".f_score", c("Surv", "SurvEvents"),
  function(observed, predicted, beta, ...) {
    .metric.Surv_matrix(observed, predicted, f_score, beta = beta)
  }
)


setMethod(".f_score", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, beta, ...) {
    .metric.Surv_matrix(observed, predicted, f_score, cutoff, beta = beta)
  }
)


#' @rdname metrics
#' 
fnr <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .fnr(observed, predicted, cutoff = cutoff)
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


setMethod(".fnr", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    1 - tpr(observed, predicted)
  }
)


setMethod(".fnr", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    1 - tpr(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
fpr <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .fpr(observed, predicted, cutoff = cutoff)
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


setMethod(".fpr", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    1 - tnr(observed, predicted)
  }
)


setMethod(".fpr", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    1 - tnr(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
kappa2 <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .kappa2(observed, predicted, cutoff = cutoff)
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


setMethod(".kappa2", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    .metric.Surv_matrix(observed, predicted, kappa2)
  }
)


setMethod(".kappa2", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    .metric.Surv_matrix(observed, predicted, kappa2, cutoff)
  }
)


#' @rdname metrics
#' 
npv <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .npv(observed, predicted, cutoff = cutoff)
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


setMethod(".npv", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    .metric.Surv_matrix(observed, predicted, npv)
  }
)


setMethod(".npv", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    .metric.Surv_matrix(observed, predicted, npv, cutoff)
  }
)


#' @rdname metrics
#' 
ppv <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .ppv(observed, predicted, cutoff = cutoff)
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


setMethod(".ppv", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    .metric.Surv_matrix(observed, predicted, ppv)
  }
)


setMethod(".ppv", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    .metric.Surv_matrix(observed, predicted, ppv, cutoff)
  }
)


#' @rdname metrics
#' 
pr_auc <- function(observed, predicted = NULL, ...) {
  .pr_auc(observed, predicted)
}

MLMetric(pr_auc) <- list("pr_auc", "Area Under Precision-Recall Curve", TRUE)


setGeneric(".pr_auc",
           function(observed, predicted, ...) standardGeneric(".pr_auc"))


setMethod(".pr_auc", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".pr_auc", c("factor", "numeric"),
  function(observed, predicted, ...) {
    auc(observed, predicted, metrics = c(precision, recall))
  }
)


setMethod(".pr_auc", c("Surv", "SurvProbs"),
  function(observed, predicted, ...) {
    auc(observed, predicted, metrics = c(precision, recall))
  }
)


#' @rdname metrics
#' 
precision <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .precision(observed, predicted, cutoff = cutoff)
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


setMethod(".precision", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    ppv(observed, predicted)
  }
)


setMethod(".precision", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    ppv(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
recall <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .recall(observed, predicted, cutoff = cutoff)
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


setMethod(".recall", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    tpr(observed, predicted)
  }
)


setMethod(".recall", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    tpr(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
roc_auc <- function(observed, predicted = NULL, ...) {
  .roc_auc(observed, predicted)
}

MLMetric(roc_auc) <- list("roc_auc", "Area Under ROC Curve", TRUE)


setGeneric(".roc_auc",
           function(observed, predicted, ...) standardGeneric(".roc_auc"))


setMethod(".roc_auc", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".roc_auc", c("factor", "numeric"),
  function(observed, predicted, ...) {
    auc(observed, predicted)
  }
)


setMethod(".roc_auc", c("Surv", "SurvProbs"),
  function(observed, predicted, ...) {
    auc(observed, predicted)
  }
)


#' @rdname metrics
#' 
roc_index <- function(observed, predicted = NULL, cutoff = 0.5,
                      f = function(sensitivity, specificity)
                        (sensitivity + specificity) / 2, ...) {
  .roc_index(observed, predicted, cutoff = cutoff, f = f)
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


setMethod(".roc_index", c("Surv", "SurvEvents"),
  function(observed, predicted, f, ...) {
    .metric.Surv_matrix(observed, predicted, roc_index, f = f)
  }
)


setMethod(".roc_index", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, f, ...) {
    .metric.Surv_matrix(observed, predicted, roc_index, cutoff, f = f)
  }
)


#' @rdname metrics
#' 
rpp <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .rpp(observed, predicted, cutoff = cutoff)
}

MLMetric(rpp) <- list("rpp", "Rate of Positive Prediction", FALSE)


setGeneric(".rpp", function(observed, predicted, ...) standardGeneric(".rpp"))


setMethod(".rpp", c("ANY", "ANY"),
  function(observed, predicted, ...) numeric()
)


setMethod(".rpp", c("ConfusionMatrix", "NULL"),
  function(observed, predicted, ...) {
    if (any(dim(observed) != c(2, 2))) {
      warn("'rpp' requires a 2-level response")
      numeric()
    } else {
      (observed[2, 1] + observed[2, 2]) / sum(observed)
    }
  }
)


setMethod(".rpp", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    rpp(confusion(observed, predicted, cutoff = cutoff))
  }
)


setMethod(".rpp", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    .metric.Surv_matrix(observed, predicted, rpp)
  }
)


setMethod(".rpp", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    .metric.Surv_matrix(observed, predicted, rpp, cutoff)
  }
)


#' @rdname metrics
#' 
sensitivity <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .sensitivity(observed, predicted, cutoff = cutoff)
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


setMethod(".sensitivity", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    tpr(observed, predicted)
  }
)


setMethod(".sensitivity", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    tpr(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
specificity <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .specificity(observed, predicted, cutoff = cutoff)
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


setMethod(".specificity", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    tnr(observed, predicted)
  }
)


setMethod(".specificity", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    tnr(observed, predicted, cutoff = cutoff)
  }
)


#' @rdname metrics
#' 
tnr <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .tnr(observed, predicted, cutoff = cutoff)
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


setMethod(".tnr", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    .metric.Surv_matrix(observed, predicted, tnr)
  }
)


setMethod(".tnr", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    .metric.Surv_matrix(observed, predicted, tnr, cutoff)
  }
)


#' @rdname metrics
#' 
tpr <- function(observed, predicted = NULL, cutoff = 0.5, ...) {
  .tpr(observed, predicted, cutoff = cutoff)
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


setMethod(".tpr", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    .metric.Surv_matrix(observed, predicted, tpr)
  }
)


setMethod(".tpr", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    .metric.Surv_matrix(observed, predicted, tpr, cutoff)
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


.metric.Surv_matrix <- function(observed, predicted, FUN, cutoff = NULL, ...) {
  conf <- confusion(observed, predicted, cutoff = cutoff)
  metrics <- sapply(conf, FUN, ...)
  times <- predicted@times
  if (length(times) > 1) {
    c("mean" = mean.SurvMetrics(metrics, times), metrics)
  } else {
    metrics[[1]]
  }
}


mean.SurvMetrics <- function(x, times) {
  weights <- diff(c(0, times)) / tail(times, 1)
  sum(weights * x)
}
