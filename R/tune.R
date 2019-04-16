MLModelTune <- function(object, tune_grid, performance, selected) {
  new("MLModelTune", object, tune_grid = tune_grid, performance = performance,
      selected = selected)
}


#' Model Tuning and Selection
#' 
#' Evaluate a model over a grid of tuning parameters or a list of specified
#' models and select the best one according to resample estimation of predictive
#' performance.
#' 
#' @name tune
#' @rdname tune-methods
#' 
#' @param x defines a relationship between model predictor and response
#' variables.  May be a \code{formula}, design matrix of predictors,
#' \code{ModelFrame}, or \code{recipe}.
#' @param ... arguments passed to the \code{metrics} functions.
#' 
#' @return \code{MLModelTune} class object that inherits from \code{MLModel}.
#' 
tune <- function(x, ...) {
  UseMethod("tune")
}


#' @rdname tune-methods
#' 
#' @param data \code{data.frame} containing observed predictors and outcomes.
#' @param models \code{MLModel} function, function name, object or list of the
#' aforementioned elements, such as that returned by \code{\link{expand.model}}.
#' @param grid \code{data.frame} containing parameter values at which to
#' evaluate a single model supplied to \code{models}, the number of
#' parameter-specific values to generate automatically if the model has a
#' pre-defined grid, or a call to \code{\link{Grid}}.  Ignored in the case of a
#' list of models.
#' @param fixed list of fixed parameter values to combine with those in
#' \code{grid}.
#' @param control \code{\link{MLControl}} object, control function, or character
#' string naming a control function defining the resampling method to be
#' employed.
#' @param metrics function, one or more function names, or list of named
#' functions to include in the calculation of performance metrics.  The default
#' \code{\link{performance}} metrics are used unless otherwise specified.  Model
#' selection is based on the first specified metric.
#' @param stat function to compute a summary statistic on resampled values of
#' the metric for model selection.
#' @param maximize logical indicating whether to select the model having the
#' maximum or minimum value of the performance metric.  Set automatically if a
#' package \code{\link{metrics}} function is explicitly specified for the model
#' selection.
#' 
#' @seealso \code{\link{ModelFrame}}, \code{\link[recipes]{recipe}},
#' \code{\link{modelinfo}}, \code{\link{expand.model}}, \code{\link{Grid}},
#' \code{\link{MLControl}}, \code{\link{fit}}, \code{\link{plot}},
#' \code{\link{summary}}
#' 
#' @examples
#' \donttest{
#' ## Numeric response example
#' fo <- sale_amount ~ .
#' 
#' # User-specified grid
#' (gbmtune1 <- tune(fo, data = ICHomes, model = GBMModel,
#'                   grid = expand.grid(n.trees = c(25, 50, 100),
#'                                      interaction.depth = 1:3,
#'                                      n.minobsinnode = c(5, 10)),
#'                   control = CVControl(folds = 10, repeats = 5)))
#' 
#' # Automatically generated grid
#' (gbmtune2 <- tune(fo, data = ICHomes, model = GBMModel, grid = 3,
#'                   control = CVControl(folds = 10, repeats = 5)))
#' 
#' # Randomly sampled grid points
#' (gbmtune3 <- tune(fo, data = ICHomes, model = GBMModel,
#'                   grid = Grid(length = 1000, random = 10),
#'                   control = CVControl(folds = 10, repeats = 5)))
#' 
#' summary(gbmtune3)
#' plot(gbmtune3, type = "line")
#' 
#' gbmfit <- fit(fo, data = ICHomes, model = gbmtune3)
#' varimp(gbmfit)
#' }
#' 
tune.formula <- function(x, data, models, grid = 3, fixed = NULL,
                         control = CVControl, metrics = NULL, stat = base::mean,
                         maximize = TRUE, ...) {
  .tune(x, data, models, grid, fixed, control, metrics, stat, maximize, ...)
}


#' @rdname tune-methods
#' 
#' @param y predictor variable.
#' 
tune.matrix <- function(x, y, models, grid = 3, fixed = NULL,
                        control = CVControl, metrics = NULL, stat = base::mean,
                        maximize = TRUE, ...) {
  .tune(x, y, models, grid, fixed, control, metrics, stat, maximize, ...)
}


#' @rdname tune-methods
#' 
tune.ModelFrame <- function(x, models, grid = 3, fixed = NULL,
                            control = CVControl, metrics = NULL,
                            stat = base::mean, maximize = TRUE, ...) {
  .tune(x, NULL, models, grid, fixed, control, metrics, stat, maximize, ...)
}


#' @rdname tune-methods
#' 
tune.recipe <- function(x, models, grid = 3, fixed = NULL, control = CVControl,
                        metrics = NULL, stat = base::mean, maximize = TRUE,
                        ...) {
  .tune(x, NULL, models, grid, fixed, control, metrics, stat, maximize, ...)
}


.tune <- function(x, data, models, grid, fixed, control, metrics, stat,
                  maximize, ...) {
  
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
    if (is(grid, "character")) grid <- get(grid, mode = "function")
    if (is(grid, "function")) grid <- grid()
    if (is(grid, "Grid")) {
      random <- grid$random
      grid <- grid$length
    }
    if (is(grid, "numeric")) {
      grid <- grid(x, data, model = model, length = grid, random = random)
    }
    grid <- combine_tune_params(grid, fixed)
    models <- expand.model(list(get(model@name, mode = "function"), grid))
  }
  
  control <- getMLObject(control, "MLControl")
  
  perf_list <- list()
  perf_stat <- numeric()
  for (name in names(models)) {
    res <- resample(x, data, model = models[[name]], control = control)
    if (is.null(metrics)) {
      method <- get(findMethod(performance, res$Observed))
      metrics <- eval(formals(method)$metrics)
      is_defined <- sapply(metrics, function(metric) {
        info <- metricinfo(metric)[[1]]
        any(mapply(is, list(res$Observed), info$types$observed) &
              mapply(is, list(res$Predicted), info$types$predicted))
      })
      metrics <- metrics[is_defined]
    }
    perf_list[[name]] <- performance(res, metrics = metrics, ...)
    perf_stat[name] <- stat(na.omit(perf_list[[name]][, 1]))
  }
  perf <- do.call(Performance, perf_list)
  
  metric <- metrics
  if (is(metric, "vector")) metric <- metric[[1]]
  if (is(metric, "character")) metric <- get(metric, mode = "function")
  if (is(metric, "MLMetric")) maximize <- metric@maximize
  selected <- ifelse(maximize, which.max, which.min)(perf_stat)
  
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
