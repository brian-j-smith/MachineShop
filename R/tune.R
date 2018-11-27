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
#' @param control \code{\linkS4class{MLControl}} object, control function, or
#' character string naming a control function defining the resampling method to
#' be employed.
#' @param metric numeric index or character name of the performance metric to
#' use in selecting the best model.
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
                         control = CVControl, metric = 1, stat = mean,
                         maximize = TRUE, ...) {
  .tune(models, grid, control, metric, stat, maximize, x, data)
}


#' @rdname tune-methods
#' 
tune.ModelFrame <- function(x, models, grid = data.frame(),
                            control = CVControl, metric = 1, stat = mean,
                            maximize = TRUE, ...) {
  .tune(models, grid, control, metric, stat, maximize, x)
}


#' @rdname tune-methods
#' 
tune.recipe <- function(x, models, grid = data.frame(),
                        control = CVControl, metric = 1, stat = mean,
                        maximize = TRUE, ...) {
  .tune(models, grid, control, metric, stat, maximize, x)
}


.tune <- function(models, grid, control, metric, stat, maximize, ...) {
  
  if (is.list(models)) {
    models <- lapply(models, getMLObject, class = "MLModel")
    grid <- data.frame()
  } else {
    models <- split(grid, seq(max(1, nrow(grid)))) %>%
      lapply(function(params) do.call(models, params))
  }
  
  control <- getMLObject(control, "MLControl")
  
  resamples <- list()
  perf <- list()
  for (i in seq(models)) {
    resamples[[i]] <- resample(..., models[[i]], control)
    perf[[i]] <- apply(modelmetrics(resamples[[i]]), 2, function(x) {
      stat(na.omit(x))
    })
  }
  perf <- as.data.frame(do.call(rbind, perf))
  metric <- match_indices(metric, names(perf))
  selected <- ifelse(maximize, which.max, which.min)(perf[[metric]])
  MLModelTune(models[[selected]], grid = grid,
              resamples = do.call(Resamples, resamples),
              selected = structure(selected, names = metric))
  
}
