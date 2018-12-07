#' Model Performance Metrics
#' 
#' Compute measures of model performance.
#' 
#' @rdname modelmetrics
#' 
#' @param x observed responses or class containing observed and predicted
#' responses.
#' @param ... arguments passed to the \code{\link{metrics}} functions and from
#' the \code{Resamples} method to the others.
#' @param y predicted responses.
#' 
modelmetrics <- function(x, ...) {
  UseMethod("modelmetrics")
}


#' @rdname modelmetrics
#' 
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating model metrics.
#' 
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{metrics}}
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' (metrics <- modelmetrics(res))
#' summary(metrics)
#' plot(metrics)
#' 
modelmetrics.Resamples <- function(x, ..., na.rm = TRUE) {
  control <- x@control
  if (na.rm) x <- na.omit(x)
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
#' @param metrics function, one or more function names, or list of named
#' functions to include in the calculation of performance metrics.
#' 
modelmetrics.factor <- function(x, y, metrics =
                                  c("Accuracy" = MachineShop::accuracy,
                                    "Kappa" = MachineShop::kappa,
                                    "ROCAUC" = MachineShop::roc_auc,
                                    "Sensitivity" = MachineShop::sensitivity,
                                    "Specificity" = MachineShop::specificity,
                                    "Brier" = MachineShop::brier), ...) {
  metrics <- list2function(metrics)
  metrics(x, y, ...)
}


#' @rdname modelmetrics
#' 
modelmetrics.matrix <- function(x, y, metrics =
                                  c("R2" = MachineShop::r2,
                                    "RMSE" = MachineShop::rmse,
                                    "MAE" = MachineShop::mae), ...) {
  metrics <- list2function(metrics)
  metrics(x, y, ...)
}


#' @rdname modelmetrics
#' 
modelmetrics.numeric <- function(x, y, metrics =
                                   c("R2" = MachineShop::r2,
                                     "RMSE" = MachineShop::rmse,
                                     "MAE" = MachineShop::mae), ...) {
  metrics <- list2function(metrics)
  metrics(x, y, ...)
}


#' @rdname modelmetrics
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
modelmetrics.Surv <- function(x, y, metrics =
                                c("CIndex" = MachineShop::cindex,
                                  "ROC" = MachineShop::roc_auc,
                                  "Brier" = MachineShop::brier), ...) {
  metrics <- list2function(metrics)
  metrics(x, y, ...)
}
