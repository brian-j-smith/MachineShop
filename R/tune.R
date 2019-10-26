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
#'   \code{\link[recipes]{recipe}}, or \code{\link{TunedRecipe}} object.
#'   Alternatively, a \link[=models]{model} function, call, or
#'   \link[=ModelList]{list} of these may be given first followed by objects
#'   defining the predictor and response relationship and the other tuning
#'   argument values.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param models \link[=models]{model} function, function name, or call defining
#'   a model to tune; or vector of these from which to select, such as that
#'   returned by \code{\link{expand_model}}.
#' @param grid \link[=data.frame]{data frame} containing parameter values at
#'   which to evaluate a single model supplied to \code{models}, such as that
#'   returned by \code{\link{expand_params}}; the number of parameter-specific
#'   values to generate automatically if the model has a pre-defined grid; or a
#'   call to \code{\link{Grid}}.  Ignored in the case of a list of models.
#' @param fixed list of fixed parameter values to combine with those in
#'   \code{grid}.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Model
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for model tuning.
#' @param ... arguments passed to the \link{performance} functions.
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
#' (gbm_tune1 <- tune(fo, data = ICHomes, model = GBMModel,
#'                    grid = expand_params(n.trees = c(25, 50, 100),
#'                                         interaction.depth = 1:3,
#'                                         n.minobsinnode = c(5, 10)),
#'                    control = CVControl(folds = 10, repeats = 5)))
#' 
#' # Automatically generated grid
#' (gbm_tune2 <- tune(fo, data = ICHomes, model = GBMModel, grid = 3,
#'                    control = CVControl(folds = 10, repeats = 5)))
#' 
#' # Randomly sampled grid points
#' (gbm_tune3 <- tune(fo, data = ICHomes, model = GBMModel,
#'                    grid = Grid(length = 1000, random = 10),
#'                    control = CVControl(folds = 10, repeats = 5)))
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
tune.formula <- function(x, data, models, grid = MachineShop::settings("grid"),
                         fixed = NULL,
                         control = MachineShop::settings("control"),
                         metrics = NULL,
                         stat = MachineShop::settings("stat.Tune"), ...) {
  .tune(x, data = data, models = models, grid = grid, fixed = fixed,
        control = control, metrics = metrics, stat = stat, ...)
}


#' @rdname tune-methods
#' 
tune.matrix <- function(x, y, models, grid = MachineShop::settings("grid"),
                        fixed = NULL,
                        control = MachineShop::settings("control"),
                        metrics = NULL,
                        stat = MachineShop::settings("stat.Tune"), ...) {
  .tune(x, data = y, models = models, grid = grid, fixed = fixed,
        control = control, metrics = metrics, stat = stat, ...)
}


#' @rdname tune-methods
#' 
tune.ModelFrame <- function(x, models, grid = MachineShop::settings("grid"),
                            fixed = NULL,
                            control = MachineShop::settings("control"),
                            metrics = NULL,
                            stat = MachineShop::settings("stat.Tune"),
                            ...) {
  .tune(x, data = NULL, models = models, grid = grid, fixed = fixed,
        control = control, metrics = metrics, stat = stat, ...)
}


#' @rdname tune-methods
#' 
tune.recipe <- function(x, models, grid = MachineShop::settings("grid"),
                        fixed = NULL,
                        control = MachineShop::settings("control"),
                        metrics = NULL,
                        stat = MachineShop::settings("stat.Tune"), ...) {
  .tune(x, data = NULL, models = models, grid = grid, fixed = fixed,
        control = control, metrics = metrics, stat = stat, ...)
}


#' @rdname tune-methods
#' 
tune.MLModel <- function(x, ...) {
  tune(..., models = x)
}


#' @rdname tune-methods
#' 
tune.MLModelFunction <- function(x, ...) {
  tune(..., models = x)
}


#' @rdname tune-methods
#' 
tune.MLModelList <- function(x, ...) {
  tune(..., models = x)
}


.tune <- function(x, data, models, grid, fixed, control, metrics, stat, ...) {
  
  if (is.list(models)) {
    models <- ModelList(models)
    grid <- tibble(.rows = length(models))
  } else {
    model <- getMLObject(models, class = "MLModel")
    
    random <- FALSE
    if (is(grid, "character")) grid <- fget(grid)
    if (is(grid, "function")) grid <- grid()
    if (is(grid, "Grid")) {
      random <- grid@random
      grid <- grid@length
    }
    grid <- if (is(grid, "numeric")) {
      grid(x, data, model = model, length = grid, random = random)
    } else as_tibble(grid)
    
    fixed <- as_tibble(fixed)
    if (nrow(fixed) > 1) stop("only single values allowed for fixed parameters")
    grid[names(fixed)] <- fixed
    grid <- grid[!duplicated(grid), ]
    
    models <- expand_model(list(fget(model@name), grid))
  }
  
  control <- getMLObject(control, "MLControl")
  metric <- if (!is.null(metrics)) getMLObject(c(metrics)[[1]], "MLMetric")
  stat <- fget(stat)
  
  perf_list <- list()
  perf_stats <- numeric()
  for (name in names(models)) {
    res <- try(
      resample(x, data, model = models[[name]], control = control),
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
    
    perf <- performance(res, metrics = metrics, ...)
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
