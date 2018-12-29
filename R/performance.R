#' Model Performance Metrics
#' 
#' Compute measures of model performance.
#' 
#' @rdname performance
#' 
#' @param x observed responses or class containing observed and predicted
#' responses.
#' @param y predicted responses.
#' @param metrics function, one or more function names, or list of named
#' functions to include in the calculation of performance metrics.
#' @param cutoff threshold above which probabilities are classified as success
#' for binary responses.
#' @param times numeric vector of follow-up times at which survival events
#' were predicted.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed from the \code{Resamples} method to the others
#' and from deprecated function \code{modelmetrics} to \code{performance}.
#' 
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{metrics}}
#' 
performance <- function(x, ...) {
  UseMethod("performance")
}


#' @rdname performance
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' (perf <- performance(res))
#' summary(perf)
#' plot(perf)
#' 
performance.Resamples <- function(x, ..., na.rm = TRUE) {
  args <- list(...)
  args$time <- x@control@surv_times

  if (na.rm) x <- na.omit(x)
  
  perf_by <- by(x, x[c("Model", "Resample")], function(x) {
    if (nrow(x)) {
      do.call(performance, c(list(x$Observed, x$Predicted), args))
    } else {
      NA
    }
  }, simplify = FALSE)
  
  perf_list <- tapply(perf_by, rep(dimnames(perf_by)$Model, dim(perf_by)[2]),
                      function(perf) {
                        perf_model <- do.call(rbind, perf)
                        rownames(perf_model) <- dimnames(perf_by)$Resample
                        perf_model
                      }, simplify = FALSE)
  
  do.call(Performance, perf_list)
}


#' @rdname performance
#' 
performance.factor <- function(x, y, metrics =
                                 c("Accuracy" = MachineShop::accuracy,
                                   "Kappa" = MachineShop::kappa2,
                                   "ROCAUC" = MachineShop::roc_auc,
                                   "Sensitivity" = MachineShop::sensitivity,
                                   "Specificity" = MachineShop::specificity,
                                   "Brier" = MachineShop::brier),
                                cutoff = 0.5, ...) {
  metrics <- list2function(metrics)
  metrics(x, y, cutoff = cutoff)
}


#' @rdname performance
#' 
performance.matrix <- function(x, y, metrics =
                                 c("R2" = MachineShop::r2,
                                   "RMSE" = MachineShop::rmse,
                                   "MAE" = MachineShop::mae), ...) {
  metrics <- list2function(metrics)
  metrics(x, y)
}


#' @rdname performance
#' 
performance.numeric <- function(x, y, metrics =
                                  c("R2" = MachineShop::r2,
                                   "RMSE" = MachineShop::rmse,
                                   "MAE" = MachineShop::mae), ...) {
  metrics <- list2function(metrics)
  metrics(x, y)
}


#' @rdname performance
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
#' performance(obs, pred)
#' 
performance.Surv <- function(x, y, metrics =
                               c("CIndex" = MachineShop::cindex,
                                 "ROC" = MachineShop::roc_auc,
                                 "Brier" = MachineShop::brier),
                              times = numeric(), ...) {
  metrics <- list2function(metrics)
  metrics(x, y, times = times)
}


#' @rdname performance
#' 
modelmetrics <- function(...) {
  depwarn("'modelmetrics' is deprecated", "use 'performance' instead")
  performance(...)
}