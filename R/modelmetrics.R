#' Model Performance Metrices
#' 
#' Compute measures of model performance.
#' 
#' @rdname modelmetrics
#' 
#' @param observed vector of observed responses.
#' @param predicted model-predicted responses.
#' @param ... arguments to be passed to or from other methods.
#' 
#' @seealso \code{\link{predict}}
#' 
setGeneric("modelmetrics", function(observed, predicted, ...) {
  standardGeneric("modelmetrics")
})


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("factor", "factor"),
  function(observed, predicted, ...) {
    ratings <- cbind(observed, predicted)
    c("Accuracy" = 1 - ce(observed, predicted),
      "Kappa" = kappa2(ratings, weight = "unweighted")$value,
      "WeightedKappa" = kappa2(ratings, weight = "equal")$value)
  }
)


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("factor", "matrix"),
  function(observed, predicted, ...) {
    n <- nlevels(observed)
    predicted <- if(n > 2) {
      metrics <- c("MLogLoss" = multinomLogLoss(observed, predicted))
      predicted <- factor(max.col(predicted), levels = 1:n,
                          labels = levels(observed))
      metrics <- c(modelmetrics(observed, predicted, ...), metrics)
    } else {
      metrics <- modelmetrics(observed, predicted[,ncol(predicted)], ...)
    }
    metrics
  }
)


multinomLogLoss <- function(observed, predicted) {
  if(!is.matrix(observed)) observed <- model.matrix(~ observed - 1)
  eps <- 1e-15
  predicted <- pmax(pmin(predicted, 1 - eps), eps)
  -sum(observed * log(predicted)) / nrow(predicted)
}


#' @rdname modelmetrics
#' 
#' @param cutoff threshold above which probabilities are classified as success.
#' @param cutoff.index function to calculate a desired sensitivity-specificity
#' tradeoff.
#' 
setMethod("modelmetrics", c("factor", "numeric"),
  function(observed, predicted, cutoff = 0.5,
           cutoff.index = function(sens, spec) sens + spec, ...) {
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


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("numeric"),
  function(observed, predicted, ...) {
    c("RMSE" = rmse(observed, predicted),
      "RSquare" = cor(observed, predicted)^2,
      "MAE" = mae(observed, predicted))
  }
)


#' @rdname modelmetrics
#' 
#' @param survtimes numeric vector of follow-up times at which survival events
#' were predicted.
#' 
setMethod("modelmetrics", c("Surv", "matrix"),
  function(observed, predicted, survtimes, ...) {
    ntimes <- length(survtimes)
    roc <- brier <- rep(NA, ntimes)
    for(i in 1:ntimes) {
      roc[i] <- rocSurv(observed, predicted[,i], survtimes[i])
      brier[i] <- brierSurv(observed, predicted[,i], survtimes[i])
    }
    if(ntimes > 1) {
      data.frame(
        ROC = meanSurvMetric(roc, survtimes),
        Brier = meanSurvMetric(brier, survtimes),
        ROCTime = I(t(roc)),
        BrierTime = I(t(brier))
      )
    } else {
      c(ROC = roc, Brier = brier)
    }
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


#' @rdname modelmetrics
#' 
setMethod("modelmetrics", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    c("CIndex" = rcorr.cens(-predicted, observed)[[1]])
  }
)
