#' Model Tuning
#' 
#' Evaluate a model over a grid of tuning parameters and select the best one
#' according to resample estimation of predictive performance.
#' 
#' @name tune
#' @rdname tune-methods
#' 
#' @param object constructor function for the model.
#' @param x defined relationship between model predictors and an outcome.  May
#' be a model.frame (data.frame) containing a formula, data, and optionally case
#' weights; a formula; or a recipe.
#' @param ... further arguments passed to other methods.
#' 
setGeneric("tune", function(object, x, ...) standardGeneric("tune"))


#' @rdname tune-methods
#' @aliases tune,function,data.frame-method
#' 
#' @param control \code{\linkS4class{MLControl}} object defining and controlling
#' the resampling method to be employed.
#' @param grid data frame containing parameter values over which to evaluate the
#' \code{object} model function.
#' @param metric numeric index or character name of the performance metric to
#' use in selecting the best model.
#' @param stat function to compute a summary statistic on resampled values of
#' the metric for model selection.  The supplied function should contain a
#' \code{na.rm} argument in its definition.
#' @param maximize logical indicating whether to select the model having the
#' maximum or minimum value of the performance metric.
#' 
#' @return MLModelTune class object that inherits from MLModel.
#' 
#' @seealso \code{\link[stats]{model.frame}}, \code{\link[recipes]{recipe}},
#' \code{\link{BootControl}}, \code{\link{CVControl}}, \code{\link{OOBControl}},
#' \code{\link{fit}}, \code{\link{resample}}
#' 
setMethod("tune", c("function", "data.frame"),
  function(object, x, control = CVControl(), grid = data.frame(), metric = 1,
           stat = mean, maximize = TRUE) {
    .tune(object, x, control, grid = grid, metric = metric, stat = stat,
          maximize = maximize)
  }
)


#' @rdname tune-methods
#' @aliases tune,function,formula-method
#' 
#' @param data data frame containing observed predictors and outcomes.
#' 
setMethod("tune", c("function", "formula"),
  function(object, x, data, control = CVControl(), grid = data.frame(),
           metric = 1, stat = mean, maximize = TRUE) {
    .tune(object, x, data, control, grid = grid, metric = metric, stat = stat,
          maximize = maximize)
  }
)


#' @rdname tune-methods
#' @aliases tune,function,recipe-method
#' 
setMethod("tune", c("function", "recipe"),
  function(object, x, control = CVControl(), grid = data.frame(), metric = 1,
           stat = mean, maximize = TRUE) {
    .tune(object, x, control, grid = grid, metric = metric, stat = stat,
          maximize = maximize)
  }
)


.tune <- function(object, ..., grid, metric, stat, maximize) {
  models <-list()
  perf <- list()
  seed <- sample.int(.Machine$integer.max, 1)
  for(i in 1:max(1, nrow(grid))) {
    set.seed(seed)
    models[[i]] <- do.call(object, grid[i, , drop = FALSE])
    perf[[i]] <- resample(models[[i]], ...) %>%
      apply(2, stat, na.rm = TRUE)
  }
  perf <- as.data.frame(do.call(rbind, perf))
  lookup <- structure(seq(perf), names = names(perf))
  if(is.na(lookup[metric])) {
    metric <- 1
    warning("specified metric not found; tuning on ", names(lookup)[metric],
            " instead")
  }
  metric <- names(lookup)[lookup[metric]]
  selected <- ifelse(maximize, which.max, which.min)(perf[[metric]])
  MLModelTune(models[[selected]], grid = grid, perf = perf,
              selected = structure(selected, names = metric))
}
