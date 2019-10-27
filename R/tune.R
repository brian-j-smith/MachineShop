#' Model Tuning and Selection
#' 
#' Predictive peformance-based tuning of a model over a grid of parameters
#' values or selection from a set of candidate models.
#' 
#' @name tune
#' @rdname tune-methods
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
#' @return \code{MLModelTune} class object that inherits from \code{MLModel}.
#' 
#' @seealso \code{\link{fit}}, \code{\link{performance}}, \code{\link{metrics}},
#' \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' \donttest{
#' ## Numeric response example
#' fo <- sale_amount ~ .
#' 
#' # User-specified grid
#' model1 <- TunedModel(GBMModel,
#'                      grid = expand_params(n.trees = c(25, 50, 100),
#'                                           interaction.depth = 1:3,
#'                                           n.minobsinnode = c(5, 10)),
#'                      control = CVControl(folds = 10, repeats = 5))
#' (gbm_tune1 <- tune(fo, data = ICHomes, model = model1))
#' 
#' # Automatically generated grid
#' model2 <- TunedModel(GBMModel, grid = 3,
#'                      control = CVControl(folds = 10, repeats = 5))
#' (gbm_tune2 <- tune(fo, data = ICHomes, model = model2))
#' 
#' # Randomly sampled grid points
#' model3 <- TunedModel(GBMModel, grid = Grid(length = 1000, random = 10),
#'                      control = CVControl(folds = 10, repeats = 5))
#' (gbm_tune3 <- tune(fo, data = ICHomes, model = model3))
#' 
#' summary(gbm_tune1)
#' plot(gbm_tune1, type = "line")
#' 
#' gbm_fit <- fit(fo, data = ICHomes, model = gbm_tune1)
#' varimp(gbm_fit)
#' }
#' 
tune <- function(x, ...) {
  UseMethod("tune")
}


#' @rdname tune-methods
#' 
tune.formula <- function(x, data, model, ...) {
  tune_depwarn(...)
  .tune(model, x, data)
}


#' @rdname tune-methods
#' 
tune.matrix <- function(x, y, model, ...) {
  tune_depwarn(...)
  .tune(model, x, y)
}


#' @rdname tune-methods
#' 
tune.ModelFrame <- function(x, model, ...) {
  tune_depwarn(...)
  .tune(model, x)
}


#' @rdname tune-methods
#' 
tune.recipe <- function(x, model, ...) {
  tune_depwarn(...)
  .tune(model, x)
}


#' @rdname tune-methods
#' 
tune.MLModel <- function(x, ...) {
  tune(..., model = x)
}


#' @rdname tune-methods
#' 
tune.MLModelFunction <- function(x, ...) {
  tune(..., model = x)
}


tune_depwarn <- function(...) {
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
            expired = Sys.Date() >= "2020-01-15")
  }
}


.tune <- function(model, ...) {
  
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
    return(tune(SelectedModel(model), ...))
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
  index <- ifelse(metric@maximize, which.max, which.min)(perf_stats)
  
  MLModelTune(models[[index]],
              tune_grid = grid,
              performance = perf,
              selected = list(
                index = index,
                value = structure(perf_stats[index], names = colnames(perf)[1])
              ),
              metric = metric)

}
