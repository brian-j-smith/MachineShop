#' Model Tuning and Selection
#' 
#' Predictive performance-based tuning of a model over a grid of parameters
#' values or selection from a set of candidate models.
#' 
#' @rdname tune_model
#' 
#' @param model \link[=models]{model} function, function name, or call.  Supply
#'   a \code{\link{SelectedModel}} or \code{\link{TunedModel}} for model
#'   selection or tuning.
#' @param ... defined relationship between model predictor and response
#'   variables to be passed to \code{\link{fit}} and \code{\link{resample}}.
#' 
#' @return \code{MLModel} class object containing the tuning results.
#' 
#' @seealso \code{\link{fit}}, \code{\link{SelectedModel}},
#' \code{\link{TunedModel}}
#' 
#' @noRd
#' 
tune_model <- function(model, ...) {
  
  if (is(model, "SelectedModel")) {
    params <- model@params
    models <- params$models
    grid <- tibble(Index = seq(models))
  } else if (is(model, "TunedModel")) {
    params <- model@params
    grid <- as.grid(params$grid, fixed = params$fixed,
                    ..., model = getMLObject(params$model, "MLModel"))
    models <- expand_model(list(params$model, grid))
  } else {
    return(tune_model(SelectedModel(model), ...))
  }
  
  metrics <- params$metrics
  metric <- if (!is.null(metrics)) getMLObject(c(metrics)[[1]], "MLMetric")
  stat <- fget(params$stat)
  
  perf_list <- list()
  perf_stats <- numeric()
  for (name in names(models)) {
    res <- try(
      resample(models[[name]], ..., control = params$control),
      silent = TRUE
    )
    
    if (is(res, "try-error")) {
      warn("tune resampling failed for ", name, " with error:\n",
           attr(res, "condition")$message)
      perf_list[[name]] <- NA
      perf_stats[name] <- NA
      next
    }
    
    if (is.null(metrics)) {
      method <- fget(findMethod(performance, res$Observed))
      metrics <- c(eval(formals(method)$metrics))
      is_defined <- sapply(metrics, function(metric) {
        info <- metricinfo(metric)[[1]]
        any(mapply(is, list(res$Observed), info$response_types$observed) &
              mapply(is, list(res$Predicted), info$response_types$predicted))
      })
      metrics <- metrics[is_defined]
      metric <- getMLObject(metrics[[1]], "MLMetric")
    }
    
    perf <- performance(res, metrics = metrics, cutoff = params$cutoff)
    perf_list[[name]] <- perf
    perf_stats[name] <- stat(na.omit(perf[, 1]))
  }
  
  failed <- is.na(perf_list)
  if (all(failed)) {
    stop("tune resampling failed for all models", call. = FALSE)
  } else if (any(failed)) {
    perf[] <- NA
    perf_list[failed] <- list(perf)
  }
  
  perf <- do.call(c, perf_list)
  selected <- ifelse(metric@maximize, which.max, which.min)(perf_stats)
  
  tuned_model <- models[[selected]]
  tuned_model@tune <- MLTune(
    grid = grid,
    performance = perf,
    selected = structure(selected, names = colnames(perf)[1]),
    values = perf_stats,
    metric = metric
  )
  tuned_model
  
}
