#' Model Performance Metrics
#' 
#' Compute measures of model performance.
#' 
#' @name performance
#' @rdname performance
#' 
#' @param x \link[=response]{observed responses}; or \link{confusion} or
#'   \link{resample} result containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.
#' @param cutoff numeric (0, 1) threshold above which binary factor
#'   probabilities are classified as events and below which survival
#'   probabilities are classified.
#' @param na.rm logical indicating whether to remove observed or predicted
#'   responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed from the \code{Resamples} method to the response
#'   type-specific methods or from the method for \code{Confusion} to
#'   \code{ConfusionMatrix}.
#' 
#' @seealso \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' (perf <- performance(res))
#' summary(perf)
#' plot(perf)
#' 
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' fo <- Surv(time, status != 2) ~ sex + age + year + thickness + ulcer
#' gbm_fit <- fit(fo, data = Melanoma, model = GBMModel)
#' 
#' obs <- response(gbm_fit, newdata = Melanoma)
#' pred <- predict(gbm_fit, newdata = Melanoma, type = "prob")
#' performance(obs, pred)
#' 
performance <- function(x, ...) {
  UseMethod("performance")
}


#' @rdname performance
#' 
performance.factor <- function(x, y, metrics =
                                 MachineShop::settings("metrics.factor"),
                               cutoff = MachineShop::settings("cutoff"),
                               na.rm = TRUE, ...) {
  .performance(x, y, metrics, na.rm, cutoff = cutoff)
}


#' @rdname performance
#' 
performance.matrix <- function(x, y, metrics =
                                 MachineShop::settings("metrics.matrix"),
                               na.rm = TRUE, ...) {
  .performance(x, y, metrics, na.rm)
}


#' @rdname performance
#' 
performance.numeric <- function(x, y, metrics =
                                  MachineShop::settings("metrics.numeric"),
                                na.rm = TRUE, ...) {
  .performance(x, y, metrics, na.rm)
}


#' @rdname performance
#' 
performance.Surv <- function(x, y, metrics =
                               MachineShop::settings("metrics.Surv"),
                             cutoff = MachineShop::settings("cutoff"),
                             na.rm = TRUE, ...) {
  .performance(x, y, metrics, na.rm, cutoff = cutoff)
}


.performance <- function(x, y, metrics, na.rm, ...) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y)
    x <- complete$x
    y <- complete$y
  }
  if (length(x)) list2function(metrics)(x, y, ...) else NA_real_
}


#' @rdname performance
#' 
performance.Confusion <- function(x, ...) {
  structure(lapply(x, performance, ...), class = "listof")
}


#' @rdname performance
#' 
performance.ConfusionMatrix <-
  function(x, metrics = MachineShop::settings("metrics.ConfusionMatrix"), ...) {
  list2function(metrics)(x)
}


#' @rdname performance
#' 
performance.Resamples <- function(x, ...) {
  perf_list <- by(x, x$Model, function(resamples) {
    Performance(performance(x@control, resamples, ...))
  }, simplify = FALSE)
  
  do.call(c, perf_list)
}


performance.MLControl <- function(x, resamples, ...) {
  perf_list <- by(resamples[c("Observed", "Predicted")], resamples$Resample,
                  function(resample) {
                    performance(resample[[1]], resample[[2]], ...)
                  }, simplify = FALSE)
  do.call(rbind, perf_list)
}


performance.MLBootOptimismControl <- function(x, resamples, ...) {
  vars <- c("Observed", "Predicted", "Boot.Observed", "Boot.Predicted",
            "Train.Predicted")
  
  test_perf_list <- list()
  boot_perf_list <- list()
  resamples_split <- split(resamples[vars], resamples$Resample)
  for (name in names(resamples_split)) {
    resample <- resamples_split[[name]]
    test_perf_list[[name]] <- performance(resample$Observed,
                                          resample$Predicted, ...)
    boot_perf_list[[name]] <- performance(resample$Boot.Observed,
                                          resample$Boot.Predicted, ...)
  }
  test_perf <- do.call(rbind, test_perf_list)
  boot_perf <- do.call(rbind, boot_perf_list)
  train_perf <- performance(resample$Observed, resample$Train.Predicted, ...)
  
  pessimism <- test_perf - boot_perf
  sweep(pessimism, 2, train_perf, "+")
}


performance.MLCVOptimismControl <- function(x, resamples, ...) {
  vars <- c("Observed", "Predicted")
  vars2 <- c("Resample", "Observed", "Train.Predicted",
             paste0("CV.Predicted.", seq_len(x@folds)))
  
  resamples_split <- split(resamples[vars], resamples$Resample)
  test_perf <- lapply(resamples_split, function(resample) {
    performance(resample$Observed, resample$Predicted, ...)
  }) %>% do.call(rbind, .)
  
  f <- function(p, obs, pred) p * performance(obs, pred, ...)
  resamples_factor <- ceiling(resamples$Resample / x@folds)
  resamples_split <- split(resamples[vars2], resamples_factor)
  cv_perf_list <- lapply(resamples_split, function(resample) {
    p <- prop.table(table(resample$Resample))
    Reduce("+", Map(f, p, resample["Observed"], resample[-(1:3)]))
  })
  cv_perf <- do.call(rbind, rep(cv_perf_list, each = x@folds))
  train_perf <- performance(resamples_split[[1]]$Observed,
                            resamples_split[[1]]$Train.Predicted, ...)
  
  pessimism <- test_perf - cv_perf
  sweep(pessimism, 2, train_perf, "+")
}
