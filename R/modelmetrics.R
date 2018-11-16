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
#' library(MASS)
#' 
#' fo <- Surv(time, status != 2) ~ sex + age + year + thickness + ulcer
#' gbmfit <- fit(fo, data = Melanoma, model = GBMModel)
#' 
#' obs <- response(fo, data = Melanoma)
#' pred <- predict(gbmfit, newdata = Melanoma, type = "prob")
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
    metrics <- modelmetrics(observed,
                            convert_response(observed, predicted), ...)
    observed <- model.matrix(~ observed - 1)
    metrics["Brier"] <- sum((observed - predicted)^2) / nrow(observed)
    metrics["MLogLoss"] <- multinomLogLoss(observed, predicted)
    metrics
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
    roc <- ROC.Surv(observed, predicted, times)
    brier <- Brier.Surv(observed, predicted, times)
    if (length(times) > 1) {
      c("ROC" = mean(roc),
        "Brier" = mean(brier),
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
    c("CIndex" = CIndex.Surv(observed, predicted))
  }
)


Brier.Surv <- function(observed, predicted, times) {
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
  attr(metrics, "times") <- times
  class(metrics) <- c("SurvMetric", "numeric")
  metrics
}


CIndex.Surv <- function(observed, predicted) {
  rcorr.cens(-predicted, observed)[[1]]
}


ROC.Surv <- function(observed, predicted, times) {
  stopifnot(ncol(predicted) == length(times))
  
  metrics <- sapply(seq(times), function(i) {
    survivalROC(observed[, "time"], observed[, "status"], 1 - predicted[, i],
                predict.time = times[i], method = "KM")$AUC
  })
  attr(metrics, "times") <- times
  class(metrics) <- c("SurvMetric", "numeric")
  metrics
}


mean.SurvMetric <- function(x) {
  times <- attr(x, "times")
  weights <- diff(c(0, times)) / tail(times, 1)
  sum(weights * x)
}


multinomLogLoss <- function(observed, predicted) {
  eps <- 1e-15
  predicted <- pmax(pmin(predicted, 1 - eps), eps)
  -sum(observed * log(predicted)) / nrow(predicted)
}
