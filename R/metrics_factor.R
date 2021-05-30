#' @rdname metrics
#'
accuracy <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("accuracy", environment())
}

MLMetric(accuracy) <- list("accuracy", "Accuracy", TRUE)


setMetric_ConfusionMatrix("accuracy",
  function(observed, predicted, ...) {
    sum(diag(observed)) / sum(observed)
  }
)


#' @rdname metrics
#'
auc <- function(
  observed, predicted = NULL, metrics = c(MachineShop::tpr, MachineShop::fpr),
  stat = MachineShop::settings("stat.Curve"), ...
) {
  call_metric_method("auc", environment())
}

MLMetric(auc) <- list("auc", "Area Under Performance Curve", TRUE)


setMetricGeneric("auc")


setMetricMethod("auc", c("factor", "factor"))


setMetricMethod("auc", c("factor", "numeric"),
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


setMetricMethod("auc", c("PerformanceCurve", "NULL"),
  function(observed, predicted, stat, ...) {
    observed <- summary(observed, stat = stat)
    by(observed, observed["Model"], function(curve) {
      curve <- na.omit(curve)
      n <- nrow(curve)
      if (n) sum(diff(curve$x) * (curve$y[-n] + curve$y[-1]) / 2) else NA
    })
  }
)


setMetricMethod("auc", c("Resamples", "NULL"),
  function(observed, predicted, metrics, ...) {
    auc@.Data <- function(...) MachineShop::auc(..., metrics = metrics)
    performance(observed, metrics = auc, ...)
  }
)


setMetricMethod("auc", c("Surv", "SurvProbs"),
  function(observed, predicted, metrics, ...) {
    x <- unname(auc(performance_curve(observed, predicted, metrics = metrics)))
    times <- predicted@times
    if (length(times) > 1) {
      c("mean" = survmetric_mean(x, times), "time" = x)
    } else {
      x
    }
  }
)


#' @rdname metrics
#'
brier <- function(observed, predicted = NULL, ...) {
  call_metric_method("brier", environment())
}

MLMetric(brier) <- list("brier", "Brier Score", FALSE)


setMetricGeneric("brier")


setMetricMethod("brier", c("factor", "factor"))


setMetricMethod("brier", c("factor", "matrix"),
  function(observed, predicted, ...) {
    observed <- model.matrix(~ observed - 1)
    sum((observed - predicted)^2) / nrow(observed)
  }
)


setMetricMethod("brier", c("factor", "numeric"),
  function(observed, predicted, ...) {
    mse(as.numeric(observed == levels(observed)[2]), predicted)
  }
)


setMetricMethod_Resamples("brier")


setMetricMethod("brier", c("Surv", "SurvProbs"),
  function(observed, predicted, ...) {
    times <- predicted@times

    observed[, "status"] <- 1 - observed[, "status"]
    cens_fit <- survfit(observed ~ 1, stype = 2, se.fit = FALSE)

    start_time <- function(x) if (is_counting(x)) x[, "start"] else -Inf
    x <- map_num(function(i) {
      time <- times[i]
      start_by <- start_time(observed) <= time
      stop_after <- time(observed) > time
      known_status <- start_by & (observed[, "status"] == 0 | stop_after)
      cens <- predict(cens_fit, pmin(time(observed), time))
      weights <- prop.table(ifelse(known_status, 1 / cens, 0))
      sum(weights * (stop_after - predicted[, i, drop = TRUE])^2)
    }, seq_along(times))

    if (length(times) > 1) {
      c("mean" = survmetric_mean(x, times), "time" = x)
    } else {
      x
    }
  }
)


#' @rdname metrics
#'
cindex <- function(observed, predicted = NULL, ...) {
  call_metric_method("cindex", environment())
}

MLMetric(cindex) <- list("cindex", "Concordance Index", TRUE)


setMetricGeneric("cindex")


setMetricMethod("cindex", c("factor", "factor"))


setMetricMethod("cindex", c("factor", "numeric"),
  function(observed, predicted, ...) {
    roc_auc(observed, predicted)
  }
)


setMetricMethod_Resamples("cindex")


setMetricMethod("cindex", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    concordance(observed ~ predicted)$concordance
  }
)


#' @rdname metrics
#'
cross_entropy <- function(observed, predicted = NULL, ...) {
  call_metric_method("cross_entropy", environment())
}

MLMetric(cross_entropy) <- list("cross_entropy", "Cross Entropy", FALSE)


setMetricGeneric("cross_entropy")


setMetricMethod("cross_entropy", c("factor", "factor"))


setMetricMethod("cross_entropy", c("factor", "matrix"),
  function(observed, predicted, ...) {
    observed <- model.matrix(~ observed - 1)
    eps <- 1e-15
    predicted <- pmax(pmin(predicted, 1 - eps), eps)
    -sum(observed * log(predicted)) / nrow(predicted)
  }
)


setMetricMethod("cross_entropy", c("factor", "numeric"),
  function(observed, predicted, ...) {
    cross_entropy(observed, cbind(1 - predicted, predicted))
  }
)


setMetricMethod_Resamples("cross_entropy")


#' @rdname metrics
#'
f_score <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"),
  beta = 1, ...
) {
  call_metric_method("f_score", environment())
}

MLMetric(f_score) <- list("f_score", "F Score", TRUE)


setMetric_BinaryConfusionMatrix("f_score",
  function(observed, predicted, beta, ...) {
    beta2 <- beta^2
    (1 + beta2) * observed[2, 2] /
      ((1 + beta2) * observed[2, 2] + beta2 * observed[1, 2] + observed[2, 1])
  }
)


#' @rdname metrics
#'
fnr <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("fnr", environment())
}

MLMetric(fnr) <- list("fnr", "False Negative Rate", FALSE)


setMetric_BinaryConfusionMatrix("fnr",
  function(observed, predicted, ...) {
    1 - tpr(observed)
  }
)


#' @rdname metrics
#'
fpr <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("fpr", environment())
}

MLMetric(fpr) <- list("fpr", "False Positive Rate", FALSE)


setMetric_BinaryConfusionMatrix("fpr",
  function(observed, predicted, ...) {
    1 - tnr(observed)
  }
)


#' @rdname metrics
#'
kappa2 <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("kappa2", environment())
}

MLMetric(kappa2) <- list("kappa2", "Cohen's Kappa", TRUE)


setMetric_ConfusionMatrix("kappa2",
  function(observed, predicted, ...) {
    p <- prop.table(observed)
    1 - (1 - sum(diag(p))) / (1 - sum(rowSums(p) * colSums(p)))
  }
)


#' @rdname metrics
#'
npv <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("npv", environment())
}

MLMetric(npv) <- list("npv", "Negative Predictive Value", TRUE)


setMetric_BinaryConfusionMatrix("npv",
  function(observed, predicted, ...) {
    observed[1, 1] / (observed[1, 1] + observed[1, 2])
  }
)


#' @rdname metrics
#'
ppv <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("ppv", environment())
}

MLMetric(ppv) <- list("ppv", "Positive Predictive Value", TRUE)


setMetric_BinaryConfusionMatrix("ppv",
  function(observed, predicted, ...) {
    observed[2, 2] / (observed[2, 1] + observed[2, 2])
  }
)


#' @rdname metrics
#'
pr_auc <- function(observed, predicted = NULL, ...) {
  call_metric_method("pr_auc", environment())
}

MLMetric(pr_auc) <- list("pr_auc", "Area Under Precision-Recall Curve", TRUE)


setMetric_auc("pr_auc", c(precision, recall))


#' @rdname metrics
#'
precision <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("precision", environment())
}

MLMetric(precision) <- list("precision", "Precision", TRUE)


setMetric_BinaryConfusionMatrix("precision",
  function(observed, predicted, ...) {
    ppv(observed)
  }
)


#' @rdname metrics
#'
recall <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("recall", environment())
}

MLMetric(recall) <- list("recall", "Recall", TRUE)


setMetric_BinaryConfusionMatrix("recall",
  function(observed, predicted, ...) {
    tpr(observed)
  }
)


#' @rdname metrics
#'
roc_auc <- function(observed, predicted = NULL, ...) {
  call_metric_method("roc_auc", environment())
}

MLMetric(roc_auc) <- list("roc_auc", "Area Under ROC Curve", TRUE)


setMetric_auc("roc_auc", c(tpr, fpr))


#' @rdname metrics
#'
roc_index <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"),
  f = function(sensitivity, specificity) (sensitivity + specificity) / 2, ...
) {
  call_metric_method("roc_index", environment())
}

MLMetric(roc_index) <- list("roc_index", "ROC Index", TRUE)


setMetric_BinaryConfusionMatrix("roc_index",
  function(observed, predicted, f, ...) {
    f(sensitivity(observed), specificity(observed))
  }
)


#' @rdname metrics
#'
rpp <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("rpp", environment())
}

MLMetric(rpp) <- list("rpp", "Rate of Positive Prediction", FALSE)


setMetric_BinaryConfusionMatrix("rpp",
  function(observed, predicted, ...) {
    (observed[2, 1] + observed[2, 2]) / sum(observed)
  }
)


#' @rdname metrics
#'
sensitivity <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("sensitivity", environment())
}

MLMetric(sensitivity) <- list("sensitivity", "Sensitivity", TRUE)


setMetric_BinaryConfusionMatrix("sensitivity",
  function(observed, predicted, ...) {
    tpr(observed)
  }
)


#' @rdname metrics
#'
specificity <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("specificity", environment())
}

MLMetric(specificity) <- list("specificity", "Specificity", TRUE)


setMetric_BinaryConfusionMatrix("specificity",
  function(observed, predicted, ...) {
    tnr(observed)
  }
)


#' @rdname metrics
#'
tnr <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("tnr", environment())
}

MLMetric(tnr) <- list("tnr", "True Negative Rate", TRUE)


setMetric_BinaryConfusionMatrix("tnr",
  function(observed, predicted, ...) {
    observed[1, 1] / (observed[1, 1] + observed[2, 1])
  }
)


#' @rdname metrics
#'
tpr <- function(
  observed, predicted = NULL, cutoff = MachineShop::settings("cutoff"), ...
) {
  call_metric_method("tpr", environment())
}

MLMetric(tpr) <- list("tpr", "True Positive Rate", TRUE)


setMetric_BinaryConfusionMatrix("tpr",
  function(observed, predicted, ...) {
    observed[2, 2] / (observed[1, 2] + observed[2, 2])
  }
)


#' @rdname metrics
#'
weighted_kappa2 <- function(observed, predicted = NULL, power = 1, ...) {
  call_metric_method("weighted_kappa2", environment())
}

MLMetric(weighted_kappa2) <-
  list("weighted_kappa2", "Weighted Cohen's Kappa", TRUE)


setMetric_OrderedConfusionMatrix("weighted_kappa2",
  function(observed, predicted, power, ...) {
    expected <- (rowSums(observed) %o% colSums(observed)) / sum(observed)
    weights <- abs(row(observed) - col(observed))^power
    1 - sum(weights * observed) / sum(weights * expected)
  }
)
