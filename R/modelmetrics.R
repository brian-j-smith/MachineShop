setGeneric("modelmetrics", function(observed, predicted, ...) {
  standardGeneric("modelmetrics")
})


setMethod("modelmetrics", c("factor", "factor"),
  function(observed, predicted, ...) {
    ratings <- cbind(observed, predicted)
    c("Accuracy" = 1 - ce(observed, predicted),
      "Kappa" = kappa2(ratings, weight = "unweighted")$value,
      "WeightedKappa" = kappa2(ratings, weight = "equal")$value)
  }
)


setMethod("modelmetrics", c("factor", "matrix"),
  function(observed, predicted, ...) {
    n <- nlevels(observed)
    predicted <- if(n > 2) {
      metrics <- c("MLogLoss" = MultiLogLoss(predicted, observed))
      predicted <- factor(max.col(predicted), levels = 1:n,
                          labels = levels(observed))
      metrics <- c(modelmetrics(observed, predicted, ...), metrics)
    } else {
      metrics <- modelmetrics(observed, predicted[,ncol(predicted)], ...)
    }
    metrics
  }
)


setMethod("modelmetrics", c("factor", "numeric"),
  function(observed, predicted, cutoff, cutoff.index, ...) {
    observed <- observed == levels(observed)[2]
    sens <- sensitivity(observed, predicted, cutoff)
    spec <- specificity(observed, predicted, cutoff)
    c("Accuracy" = 1 - ce(observed, predicted > cutoff),
      "Kappa" = kappa(observed, predicted, cutoff),
      "Brier" = brier(observed, predicted),
      "ROCAUC" = auc(observed, predicted),
      "PRAUC" = PRAUC(predicted, observed),
      "Sensitivity" = sens,
      "Specificity" = spec,
      "Index" = cutoff.index(sens, spec))
  }
)


setMethod("modelmetrics", c("numeric"),
  function(observed, predicted, ...) {
    c("RMSE" = rmse(observed, predicted),
      "RSquare" = cor(observed, predicted)^2,
      "MAE" = mae(observed, predicted))
  }
)


setMethod("modelmetrics", c("Surv", "matrix"),
  function(observed, predicted, survtimes, ...) {
    ntimes <- length(survtimes)
    roc <- brier <- rep(NA, ntimes)
    for(i in 1:ntimes) {
      roc[i] <- rocSurv(observed, predicted[,i], survtimes[i])
      brier[i] <- brierSurv(observed, predicted[,i], survtimes[i])
    }
    statnames <- c("ROC", "Brier")
    if(ntimes > 1) {
      roc <- c(meanSurvMetric(roc, survtimes), roc)
      brier <- c(meanSurvMetric(brier, survtimes), brier)
      statnames <- c(statnames, paste(rep(statnames, each = ntimes),
                                      1:ntimes, sep = ".t"))
    }
    structure(c(roc[1], brier[1], roc[-1], brier[-1]), names = statnames)
  }
)


rocSurv <- function(observed, predicted, time) {
  survivalROC(observed[,"time"], observed[,"status"], 1 - predicted,
              predict.time = time, method = "KM")$AUC
}


brierSurv <- function(observed, predicted, time) {
  obs_times <- observed[,"time"]
  obs_events <- observed[,"status"]
  fitcens <- survfit(Surv(obs_times, 1 - obs_events) ~ 1)
  is_obs_after <- obs_times > time
  weights <- (obs_events == 1 | is_obs_after) /
    predict(fitcens, pmin(obs_times, time))
  mean(weights * (is_obs_after - predicted)^2)
}


meanSurvMetric <- function(x, times) {
  weights <- diff(c(0, times)) / tail(times, 1)
  sum(weights * x)
}


setMethod("modelmetrics", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    c("CIndex" = rcorr.cens(-predicted, observed)[[1]])
  }
)
