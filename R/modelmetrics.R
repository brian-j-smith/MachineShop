#' Model Performance Metrices
#' 
#' Compute measures of model performance.
#' 
#' @rdname modelmetrics
#' 
#' @param observed vector of observed responses.
#' @param predicted model-predicted responses.
#' @param ... arguments passed to or from other methods.
#' 
#' @seealso \code{\link{predict}}, \code{\linkS4class{MLControl}}
#' 
#' @examples
#' ## Survival response example
#' library(survival)
#' 
#' fo <- Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno +
#'                            meal.cal + wt.loss
#' gbmfit <- fit(fo, lung, GBMModel)
#' 
#' obs <- response(fo, lung)
#' pred <- predict(gbmfit, newdata = lung, type = "prob")
#' modelmetrics(obs, pred)
#' 
setGeneric("modelmetrics",
           function(observed, predicted, ...) standardGeneric("modelmetrics"))


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("factor", "factor"),
  function(observed, predicted, ...) {
    ratings <- cbind(observed, predicted)
    metrics <- c("Accuracy" = 1 - ce(observed, predicted),
                 "Kappa" = kappa2(ratings, weight = "unweighted")$value)
    if (is.ordered(observed)) {
      metrics["WeightedKappa"] <- kappa2(ratings, weight = "equal")$value
    }
    metrics
  }
)


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("factor", "matrix"),
  function(observed, predicted, ...) {
    if (nlevels(observed) > 2) {
      metrics <- modelmetrics(observed, convert(observed, predicted), ...)
      observed <- model.matrix(~ observed - 1)
      metrics["Brier"] <- sum((observed - predicted)^2) / nrow(observed)
      metrics["MLogLoss"] <- multinomLogLoss(observed, predicted)
      metrics
    } else {
      modelmetrics(observed, predicted[, ncol(predicted)], ...)
    }
  }
)


#' @rdname modelmetrics
#' 
#' @param cutoff threshold above which probabilities are classified as success.
#' @param cutoff_index function to calculate a desired sensitivity-specificity
#' tradeoff.
#' 
setMethod("modelmetrics", c("factor", "numeric"),
  function(observed, predicted, cutoff = 0.5,
           cutoff_index = function(sens, spec) sens + spec, ...) {
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
      "Index" = cutoff_index(sens, spec))
  }
)


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    stopifnot(ncol(observed) == ncol(predicted))
    sapply(1:ncol(observed), function(i) {
      modelmetrics(observed[, i], predicted[, i], ...)
    }) %>% rowMeans
  }
)


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("numeric", "matrix"),
  function(observed, predicted, ...) {
    stopifnot(ncol(predicted) == 1)
    modelmetrics(observed, drop(predicted), ...)
  }
)


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    c("R2" =
        1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2),
      "RMSE" = rmse(observed, predicted),
      "MAE" = mae(observed, predicted))
  }
)


#' @rdname modelmetrics
#' 
#' @param times numeric vector of follow-up times at which survival events
#' were predicted.
#' 
setMethod("modelmetrics", c("Surv", "matrix"),
  function(observed, predicted, times, ...) {
    ntimes <- length(times)
    roc <- brier <- rep(NA, ntimes)
    for (i in 1:ntimes) {
      roc[i] <- rocSurv(observed, predicted[, i], times[i])
      brier[i] <- brierSurv(observed, predicted[, i], times[i])
    }
    if (ntimes > 1) {
      c("ROC" = meanSurvMetric(roc, times),
        "Brier" = meanSurvMetric(brier, times),
        "ROCTime" = roc,
        "BrierTime" = brier)
    } else {
      c("ROC" = roc, "Brier" = brier)
    }
  }
)


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    c("CIndex" = rcorr.cens(-predicted, observed)[[1]])
  }
)


brierSurv <- function(observed, predicted, time) {
  obs_times <- observed[, "time"]
  obs_events <- observed[, "status"]
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


multinomLogLoss <- function(observed, predicted) {
  eps <- 1e-15
  predicted <- pmax(pmin(predicted, 1 - eps), eps)
  -sum(observed * log(predicted)) / nrow(predicted)
}


rocSurv <- function(observed, predicted, time) {
  survivalROC(observed[, "time"], observed[, "status"], 1 - predicted,
              predict.time = time, method = "KM")$AUC
}
