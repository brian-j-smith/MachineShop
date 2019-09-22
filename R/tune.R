#' Model Tuning and Selection
#' 
#' Predictive peformance-based tuning of a model over a grid of parameters
#' values or selection from a set of candidate models.
#' 
#' @name tune
#' @rdname tune-methods
#' 
#' @param x defines a relationship between model predictor and response
#' variables.  May be a \code{\link{formula}}, design \code{\link{matrix}} of
#' predictors, \code{\link{ModelFrame}}, or untrained
#' \code{\link[recipes]{recipe}}.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#' outcomes.
#' @param models \link[=models]{model} function, function name, or call defining
#' a model to tune; or vector of these from which to select, such as that
#' returned by \code{\link{expand.model}}.
#' @param grid \link[=data.frame]{data frame} containing parameter values at
#' which to evaluate a single model supplied to \code{models}, the number of
#' parameter-specific values to generate automatically if the model has a
#' pre-defined grid, or a call to \code{\link{Grid}}.  Ignored in the case of a
#' list of models.
#' @param fixed list of fixed parameter values to combine with those in
#' \code{grid}.
#' @param control \link[=controls]{control} function, function name, or call
#' defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#' these with which to calculate performance.  If not specified, default metrics
#' defined in the \link{performance} functions are used.  Model selection is
#' based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#' summary statistic on resampled metric values for model tuning.
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
#'                    grid = expand.grid(n.trees = c(25, 50, 100),
#'                                       interaction.depth = 1:3,
#'                                       n.minobsinnode = c(5, 10)),
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
                         stat = MachineShop::settings("stat.ModelTune"), ...) {
  .tune(x, data, models, grid, fixed, control, metrics, stat, ...)
}


#' @rdname tune-methods
#' 
tune.matrix <- function(x, y, models, grid = MachineShop::settings("grid"),
                        fixed = NULL,
                        control = MachineShop::settings("control"),
                        metrics = NULL,
                        stat = MachineShop::settings("stat.ModelTune"), ...) {
  .tune(x, y, models, grid, fixed, control, metrics, stat, ...)
}


#' @rdname tune-methods
#' 
tune.ModelFrame <- function(x, models, grid = MachineShop::settings("grid"),
                            fixed = NULL,
                            control = MachineShop::settings("control"),
                            metrics = NULL,
                            stat = MachineShop::settings("stat.ModelTune"),
                            ...) {
  .tune(x, NULL, models, grid, fixed, control, metrics, stat, ...)
}


#' @rdname tune-methods
#' 
tune.recipe <- function(x, models, grid = MachineShop::settings("grid"),
                        fixed = NULL,
                        control = MachineShop::settings("control"),
                        metrics = NULL,
                        stat = MachineShop::settings("stat.ModelTune"), ...) {
  .tune(x, NULL, models, grid, fixed, control, metrics, stat, ...)
}


MLModelTune <- function(object, tune_grid, performance, selected) {
  new("MLModelTune", object, tune_grid = tune_grid, performance = performance,
      selected = selected)
}


.tune <- function(x, data, models, grid, fixed, control, metrics, stat, ...) {
  
  if (is.list(models)) {
    model_names <- character()
    for (i in seq(models)) {
      models[[i]] <- getMLObject(models[[i]], class = "MLModel")
      name <- names(models)[i]
      model_names[i] <- 
        if (!is.null(name) && nzchar(name)) name else models[[i]]@name
    }
    names(models) <- make.unique(model_names)
    grid <- data.frame()
  } else {
    model <- getMLObject(models, class = "MLModel")
    random <- FALSE
    if (is(grid, "character")) grid <- fget(grid)
    if (is(grid, "function")) grid <- grid()
    if (is(grid, "Grid")) {
      random <- grid$random
      grid <- grid$length
    }
    if (is(grid, "numeric")) {
      grid <- grid(x, data, model = model, length = grid, random = random)
    }
    grid <- combine_tune_params(grid, fixed)
    models <- expand.model(list(fget(model@name), grid))
  }
  
  control <- getMLObject(control, "MLControl")
  metric <- if (!is.null(metrics)) getMLObject(c(metrics)[[1]], "MLMetric")
  stat <- fget(stat)
  
  perf_list <- list()
  perf_stat <- numeric()
  for (name in names(models)) {
    res <- try(
      resample(x, data, model = models[[name]], control = control),
      silent = TRUE
    )
    
    if (is(res, "try-error")) {
      warn("tune resampling failed for ", name, " with error:\n",
           attr(res, "condition")$message)
      perf_list[[name]] <- NA
      perf_stat[name] <- NA
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
    perf_stat[name] <- stat(na.omit(perf[, 1]))
  }
  
  failed <- is.na(perf_list)
  if (all(failed)) {
    stop("tune resampling failed for all models", call. = FALSE)
  } else if (any(failed)) {
    perf[] <- NA
    perf_list[failed] <- list(perf)
  }
  
  perf <- do.call(Performance, perf_list)
  selected <- ifelse(metric@maximize, which.max, which.min)(perf_stat)
  
  MLModelTune(models[[selected]], tune_grid = grid, performance = perf,
              selected = structure(selected, names = colnames(perf)[1]))
  
}


combine_tune_params <- function(grid, fixed) {
  fixed <- as.data.frame(fixed, stringsAsFactors = FALSE)
  if (nrow(fixed) > 1) stop("only single values allowed for fixed parameters")
  
  grid_params <- names(grid)
  fixed_params <- names(fixed)
  common_params <- intersect(grid_params, fixed_params)
  new_params <- setdiff(fixed_params, common_params)
  
  grid[common_params] <- fixed[common_params]
  grid <- grid[!duplicated(grid), , drop = FALSE]
  rownames(grid) <- NULL
  
  grid[new_params] <- fixed[new_params]
  
  grid
}
