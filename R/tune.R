#' Model Tuning
#' 
#' Evaluate a model over a grid of tuning parameters and select the best one
#' according to resample estimation of predictive performance.
#' 
#' @name tune
#' @rdname tune-methods
#' 
#' @param x defined relationship between model predictors and an outcome.  May
#' be a model.frame (data.frame) containing a formula, data, and optionally case
#' weights; a formula; or a recipe.
#' @param ... arguments passed to other methods.
#' 
tune <- function(x, ...) {
  UseMethod("tune", x)
}


#' @rdname tune-methods
#' 
tune.data.frame <- function(x, model, grid = data.frame(),
                            control = CVControl, metric = 1, stat = mean,
                            maximize = TRUE, ...) {
  .tune(model, grid, control, metric, stat, maximize, x)
}


#' @rdname tune-methods
#' 
#' @param data data frame containing observed predictors and outcomes.
#' @param model constructor function or character string naming a constructor
#' function that returns an MLModel object.
#' @param grid data frame containing parameter values over which to evaluate the
#' \code{model} constructor function.
#' @param control \code{\linkS4class{MLControl}} object, control function, or
#' character string naming a control function defining the resampling method to
#' be employed.
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
#' \donttest{
#' ## Survival analysis example
#' library(survival)
#' 
#' fo <- Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno +
#'                            meal.cal + wt.loss
#' 
#' (gbmtune <- tune(fo, data = lung, model = GBMModel,
#'                  grid = expand.grid(n.trees = c(25, 50, 100),
#'                                     interaction.depth = 1:3,
#'                                     n.minobsinnode = c(5, 10)),
#'                  control = CVControl(folds = 10, repeats = 5,
#'                                      surv_times = c(180, 360, 540))))
#' summary(gbmtune)
#' plot(gbmtune, type = "line", metrics = c("ROC", "Brier"))
#' 
#' gbmfit <- fit(fo, data = lung, gbmtune)
#' (vi <- varimp(gbmfit))
#' plot(vi)
#' }
#' 
tune.formula <- function(x, data, model, grid = data.frame(),
                         control = CVControl, metric = 1, stat = mean,
                         maximize = TRUE, ...) {
  .tune(model, grid, control, metric, stat, maximize, x, data)
}


#' @rdname tune-methods
#' 
tune.recipe <- function(x, model, grid = data.frame(),
                        control = CVControl, metric = 1, stat = mean,
                        maximize = TRUE, ...) {
  .tune(model, grid, control, metric, stat, maximize, x)
}


.tune <- function(model, grid, control, metric, stat, maximize, ...) {
  control <- getMLObject(control, "MLControl")
  models <-list()
  resamples <- list()
  perf <- list()
  for (i in 1:max(1, nrow(grid))) {
    models[[i]] <- do.call(model, grid[i, , drop = FALSE])
    resamples[[i]] <- resample(..., models[[i]], control)
    perf[[i]] <- resamples[[i]] %>%
      apply(2, stat, na.rm = TRUE)
  }
  perf <- as.data.frame(do.call(rbind, perf))
  metric <- match_indices(metric, names(perf))
  selected <- ifelse(maximize, which.max, which.min)(perf[[metric]])
  MLModelTune(models[[selected]], grid = grid,
              resamples = do.call(Resamples, resamples),
              selected = structure(selected, names = metric))
}
