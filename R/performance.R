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
#' @param cutoff threshold above which binary factor probabilities are
#' classified as events and below which survival probabilities are classified.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed from the \code{Resamples} method to the response
#' type-specific methods or from the method for \code{Confusion} to
#' \code{ConfusionMatrix}.
#' 
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{confusion}}, \code{\link{metrics}},
#' \code{\link{plot}}, \code{\link{summary}}
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
  if (na.rm) x <- na.omit(x)
  
  perf_by <- by(x, x[c("Model", "Resample")], function(x) {
    if (nrow(x)) performance(x$Observed, x$Predicted, ...) else NA
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
                                 c("Brier" = MachineShop::brier,
                                   "Accuracy" = MachineShop::accuracy,
                                   "Kappa" = MachineShop::kappa2,
                                   "Weighted Kappa" =
                                     MachineShop::weighted_kappa2,
                                   "ROCAUC" = MachineShop::roc_auc,
                                   "Sensitivity" = MachineShop::sensitivity,
                                   "Specificity" = MachineShop::specificity),
                                cutoff = 0.5, ...) {
  metrics <- list2function(metrics)
  metrics(x, y, cutoff = cutoff)
}


#' @rdname performance
#' 
performance.matrix <- function(x, y, metrics =
                                 c("RMSE" = MachineShop::rmse,
                                   "R2" = MachineShop::r2,
                                   "MAE" = MachineShop::mae), ...) {
  metrics <- list2function(metrics)
  metrics(x, y)
}


#' @rdname performance
#' 
performance.numeric <- function(x, y, metrics =
                                  c("RMSE" = MachineShop::rmse,
                                    "R2" = MachineShop::r2,
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
                                 "Brier" = MachineShop::brier,
                                 "ROCAUC" = MachineShop::roc_auc,
                                 "Accuracy" = MachineShop::accuracy),
                              cutoff = 0.5, ...) {
  metrics <- list2function(metrics)
  metrics(x, y, cutoff = cutoff)
}


#' @rdname performance
#' 
performance.Confusion <- function(x, ...) {
  structure(lapply(x, performance, ...), class = "listof")
}


#' @rdname performance
#' 
performance.ConfusionMatrix <- function(x, metrics =
                                          c("Accuracy" = MachineShop::accuracy,
                                            "Kappa" = MachineShop::kappa2),
                                        ...) {
  metrics <- list2function(metrics)
  metrics(x)
}
