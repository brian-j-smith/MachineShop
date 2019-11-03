#' Model Tuning and Selection
#' 
#' Predictive peformance-based tuning of a model over a grid of parameters
#' values or selection from a set of candidate models.
#' 
#' @rdname tune_model
#' 
#' @param x defines a relationship between model predictor and response
#'   variables.  May be a \code{\link{formula}}, design \code{\link{matrix}} of
#'   predictors, \code{\link{ModelFrame}}, untrained
#'   \code{\link[recipes]{recipe}}, \code{\link{SelectedRecipe}}, or
#'   \code{\link{TunedRecipe}} object. Alternatively, a \link[=models]{model}
#'   function or call may be given first followed by objects defining the
#'   predictor and response relationship and the other tuning argument values.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or call.  Supply
#'   a \code{\link{SelectedModel}} object for model selection or a
#'   \code{\link{TunedModel}} object for model tuning.
#' @param ... arguments passed to other methods.
#' 
#' @details
#' The \code{tune} function is intended for internal use by the package and will
#' be deprecated in the future.  To perform model tuning or selection, users
#' should call the \code{\link{fit}} function with a \code{\link{TunedModel}} or
#' \code{\link{SelectedModel}} instead.
#' 
#' @return \code{MLModel} class object containing the tuning results.
#' 
#' @seealso \code{\link{fit}}, \code{\link{SelectedModel}},
#' \code{\link{TunedModel}}
#' 
#' @noRd
#' 
tune_model <- function(x, ...) {
  UseMethod("tune_model")
}


#' @rdname tune_model
#' 
tune_model.formula <- function(x, data, model, ...) {
  tune_model_depwarn(...)
  .tune_model(model, x, data)
}


#' @rdname tune_model
#' 
tune_model.matrix <- function(x, y, model, ...) {
  tune_model_depwarn(...)
  .tune_model(model, x, y)
}


#' @rdname tune_model
#' 
tune_model.ModelFrame <- function(x, model, ...) {
  tune_model_depwarn(...)
  .tune_model(model, x)
}


#' @rdname tune_model
#' 
tune_model.recipe <- function(x, model, ...) {
  tune_model_depwarn(...)
  .tune_model(model, x)
}


#' @rdname tune_model
#' 
tune_model.MLModel <- function(x, ...) {
  tune_model(..., model = x)
}


#' @rdname tune_model
#' 
tune_model.MLModelFunction <- function(x, ...) {
  tune_model(..., model = x)
}


tune_model_depwarn <- function(...) {
  args <- list(...)
  
  if (!is.null(args$models)) {
    depwarn("'models' argument to tune is deprecated",
            "supply a SelectedModel or TunedModel to 'model' instead",
            expired = TRUE)
  }
  
  dep_names <- c("grid", "fixed", "control", "metrics", "stat")
  if (any(pmatch(names(args), dep_names, nomatch = 0))) {
    depwarn(paste("deprecated tune arguments:", toString(dep_names)),
            "supply a SelectedModel or TunedModel instead",
            expired = Sys.Date() >= "2020-01-01")
  }
}


.tune_model <- function(model, ...) {
  
  if (is(model, "SelectedModel")) {
    params <- model@params
    models <- params$models
    grid <- tibble(.rows = length(models))
  } else if (is(model, "TunedModel")) {
    params <- model@params
    grid <- as.grid(params$grid, fixed = params$fixed,
                    ..., model = params$model)
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
  
  perf <- do.call(Performance, perf_list)
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
