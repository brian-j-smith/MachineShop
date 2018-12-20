#' Model Tuning and Selection
#' 
#' Evaluate a model over a grid of tuning parameters or a list of specified
#' models and select the best one according to resample estimation of predictive
#' performance.
#' 
#' @name tune
#' @rdname tune-methods
#' 
#' @param x defined relationship between model predictors and an outcome.  May
#' be a \code{ModelFrame} containing a formula, data, and optionally case
#' weights; a \code{formula}; or a \code{recipe.}
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
#' aforementioned elements such as that returned by \code{\link{expand.model}}.
#' @param grid \code{data.frame} containing parameter values over which to
#' evaluate \code{models} when a single constructor is specified.  Ignored in
#' the case of a list of models.
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
#' \code{\link{modelinfo}}, \code{\link{expand.model}}, \code{\link{MLControl}},
#' \code{\link{fit}}, \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' \donttest{
#' ## Survival response example
#' library(MASS)
#' 
#' fo <- medv ~ .
#' 
#' (gbmtune <- tune(fo, data = Boston, model = GBMModel,
#'                  grid = expand.grid(n.trees = c(25, 50, 100),
#'                                     interaction.depth = 1:3,
#'                                     n.minobsinnode = c(5, 10)),
#'                  control = CVControl(folds = 10, repeats = 5)))
#' summary(gbmtune)
#' plot(gbmtune, type = "line")
#' 
#' gbmfit <- fit(fo, data = Boston, model = gbmtune)
#' varimp(gbmfit)
#' }
#' 
tune.formula <- function(x, data, models, grid = data.frame(),
                         control = CVControl, metrics = NULL, stat = mean,
                         maximize = TRUE, ...) {
  .tune(x, data, models, grid, control, metrics, stat, maximize, ...)
}


#' @rdname tune-methods
#' 
tune.ModelFrame <- function(x, models, grid = data.frame(),
                            control = CVControl, metrics = NULL, stat = mean,
                            maximize = TRUE, ...) {
  .tune(x, NULL, models, grid, control, metrics, stat, maximize, ...)
}


#' @rdname tune-methods
#' 
tune.recipe <- function(x, models, grid = data.frame(),
                        control = CVControl, metrics = NULL, stat = mean,
                        maximize = TRUE, ...) {
  .tune(x, NULL, models, grid, control, metrics, stat, maximize, ...)
}


.tune <-
  function(x, data, models, grid, control, metrics, stat, maximize, ...) {
  
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
    models <- expand.model(list(get(model@name, mode = "function"), grid))
  }
  
  control <- getMLObject(control, "MLControl")
  
  performance_tune <-
    ifelse(is.null(metrics),
           function(x) performance(x, ...),
           function(x) performance(x, metrics = metrics, ...))

  perf_list <- list()
  perf_stat <- numeric()
  for (name in names(models)) {
    res <- resample(x, data = data, model = models[[name]], control = control)
    perf_list[[name]] <- performance_tune(res)
    perf_stat[name] <- stat(na.omit(perf_list[[name]][, 1]))
  }
  perf <- do.call(Performance, perf_list)
  
  metric <- metrics
  if (is(metric, "vector")) metric <- metric[[1]]
  if (is(metric, "character")) metric <- get(metric, mode = "function")
  if (is(metric, "MLMetric")) maximize <- metric@maximize
  selected <- ifelse(maximize, which.max, which.min)(perf_stat)
  
  MLModelTune(models[[selected]], grid = grid, performance = perf,
              selected = structure(selected, names = colnames(perf)[1]))
  
}
