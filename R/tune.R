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
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{plot}},
#' \code{\link{summary}}
#' 
#' @examples
#' ## Survival analysis example
#' library(survival)
#' 
#' fo <- Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno +
#'                            meal.cal + wt.loss
#' 
#' (gbmtune <- tune(GBMModel, fo, data = lung,
#'                  control = CVControl(folds = 10, repeats = 5,
#'                                      survtimes = 365 * c(0.5, 1, 1.5)),
#'                  grid = expand.grid(n.trees = c(100, 200, 300),
#'                                     interaction.depth = 1:3,
#'                                     n.minobsinnode = c(5, 10))))
#' summary(gbmtune)
#' plot(gbmtune, type = "line", metrics = c("ROC", "Brier"))
#' 
#' gbmfit <- fit(gbmtune, fo, data = lung)
#' (vi <- varimp(gbmfit))
#' plot(vi)
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
  resamples <- list()
  perf <- list()
  seed <- sample.int(.Machine$integer.max, 1)
  for(i in 1:max(1, nrow(grid))) {
    set.seed(seed)
    models[[i]] <- do.call(object, grid[i, , drop = FALSE])
    resamples[[i]] <- resample(models[[i]], ...)
    perf[[i]] <- resamples[[i]] %>%
      apply(2, stat, na.rm = TRUE)
  }
  perf <- as.data.frame(do.call(rbind, perf))
  metric <- match.indices(metric, names(perf))
  selected <- ifelse(maximize, which.max, which.min)(perf[[metric]])
  MLModelTune(models[[selected]], grid = grid,
              resamples = do.call(Resamples, resamples),
              selected = structure(selected, names = metric))
}
