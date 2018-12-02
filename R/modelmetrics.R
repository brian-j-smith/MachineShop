#' Model Performance Metrics
#' 
#' Compute measures of model performance.
#' 
#' @rdname modelmetrics
#' 
#' @param x observed responses or class containing observed and predicted
#' responses.
#' @param y predicted responses.
#' @param ... arguments passed to or from other methods.
#' 
modelmetrics <- function(x, ...) {
  UseMethod("modelmetrics")
}


#' @rdname modelmetrics
#' 
#' @seealso \code{\link{metrics}}, \code{\link{resample}},
#' \code{\link{Resamples}}
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' (metrics <- modelmetrics(res))
#' summary(metrics)
#' plot(metrics)
#' 
modelmetrics.Resamples <- function(x, ...) {
  control <- x@control
  if (control@na.rm) x <- na.omit(x)
  args <- list(...)
  args$times <- control@surv_times
  metrics_by <- by(x, x[c("Model", "Resample")], function(x) {
    if (nrow(x)) {
      do.call(modelmetrics, c(list(x$Observed, x$Predicted), args))
    } else {
      NA
    }
  }, simplify = FALSE)
  metrics_list <- tapply(metrics_by,
                         rep(dimnames(metrics_by)$Model, dim(metrics_by)[2]),
                         function(metrics) do.call(rbind, metrics),
                         simplify = FALSE)
  metrics <- if (length(metrics_list) > 1) {
    abind(metrics_list, along = 3)
  } else {
    metrics_list[[1]]
  }
  dimnames(metrics)[[1]] <- dimnames(metrics_by)$Resample
  ModelMetrics(metrics)
}


#' @rdname modelmetrics
#' 
#' @param metrics function or list of named functions to include in the
#' calculation of performance metrics.
#' @param cutoff threshold above which probabilities are classified as success
#' for binary responses.
#' @param cutoff_index function to calculate a desired sensitivity-specificity
#' tradeoff.
#' 
modelmetrics.factor <- function(x, y,
                                metrics = c("Accuracy" = accuracy,
                                            "Kappa" = kappa,
                                            "WeightedKappa" = weighted_kappa,
                                            "Brier" = brier,
                                            "CrossEntropy" = cross_entropy,
                                            "ROCAUC" = roc_auc,
                                            "PRAUC" = pr_auc,
                                            "Sensitivity" = sensitivity,
                                            "Specificity" = specificity,
                                            "Index" = roc_index),
                                cutoff = 0.5, cutoff_index =
                                  function(sens, spec) sens + spec, ...) {
  metrics <- list2function(metrics)
  metrics(x, y, cutoff = cutoff, cutoff_index = cutoff_index)
}


#' @rdname modelmetrics
#' 
modelmetrics.matrix <- function(x, y,
                                metrics = c("R2" = r2,
                                            "RMSE" = rmse,
                                            "MAE" = mae), ...) {
  metrics <- list2function(metrics)
  metrics(x, y)
}


#' @rdname modelmetrics
#' 
modelmetrics.numeric <- function(x, y,
                                 metrics = c("R2" = r2,
                                             "RMSE" = rmse,
                                             "MAE" = mae), ...) {
  metrics <- list2function(metrics)
  metrics(x, y)
}


#' @rdname modelmetrics
#' 
#' @param times numeric vector of follow-up times at which survival events
#' were predicted.
#' 
#' @seealso \code{\link{predict}}, \code{\link{response}}
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
modelmetrics.Surv <- function(x, y,
                              metrics = c("CIndex" = cindex,
                                          "ROC" = roc_auc,
                                          "Brier" = brier),
                              times = numeric(), ...) {
  metrics <- list2function(metrics)
  metrics(x, y, times = times)
}
