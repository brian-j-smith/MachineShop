#' Model Tuning and Selection
#' 
#' Evaluate a model over a grid of tuning parameters or a list of specified
#' model objects and select the best one according to resample estimation of
#' predictive performance.
#' 
#' @name tune
#' @rdname tune-methods
#' 
#' @param x defined relationship between model predictors and an outcome.  May
#' be a \code{ModelFrame} containing a formula, data, and optionally case
#' weights; a \code{formula}; or a \code{recipe.}
#' @param ... arguments passed to other methods.
#' 
#' @return \code{MLModelTune} class object that inherits from \code{MLModel}.
#' 
tune <- function(x, ...) {
  UseMethod("tune")
}


#' @rdname tune-methods
#' 
#' @param data \code{data.frame} containing observed predictors and outcomes.
#' @param models \code{MLModel} constructor function or character string or a
#' list of \code{MLModel} contructors or objects.
#' @param grid \code{data.frame} containing parameter values over which to
#' evaluate \code{models} when a single constructor is specified.  Ignored in
#' the case of a list of models.
#' @param control \code{\link{MLControl}} object, control function, or character
#' string naming a control function defining the resampling method to be
#' employed.
#' @param metrics function, one or more function names, or list of named
#' functions to include in the calculation of performance metrics.  The default
#' \code{\link{modelmetrics}} are used unless otherwise specified.  Model
#' selection is based on the first specified metric.
#' @param stat function to compute a summary statistic on resampled values of
#' the metric for model selection.
#' @param maximize logical indicating whether to select the model having the
#' maximum or minimum value of the performance metric.
#' 
#' @seealso \code{\link{ModelFrame}}, \code{\link[recipes]{recipe}},
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{plot}},
#' \code{\link{summary}}
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


.tune <- function(x, data, models, grid, control, metrics, stat, maximize,
                  ...) {
  
  if (is.list(models)) {
    models <- lapply(models, getMLObject, class = "MLModel")
    grid <- data.frame()
  } else {
    models <- split(grid, seq(max(1, nrow(grid)))) %>%
      lapply(function(params) do.call(models, params))
  }
  
  control <- getMLObject(control, "MLControl")
  
  modelmetrics_tune <-
    ifelse(is.null(metrics),
           function(x) modelmetrics(x, ...),
           function(x) modelmetrics(x, metrics = metrics, ...))

  resamples <- list()
  perf <- numeric()
  for (i in seq(models)) {
    resamples[[i]] <- resample(x, data = data, model = models[[i]],
                               control = control)
    modmets <- modelmetrics_tune(resamples[[i]])
    perf[i] <- stat(na.omit(modmets[, 1]))
  }
  selected <- ifelse(maximize, which.max, which.min)(perf)
  
  MLModelTune(models[[selected]], grid = grid,
              resamples = do.call(Resamples, resamples),
              selected = structure(selected, names = colnames(modmets)[1]))
  
}
