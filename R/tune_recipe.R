#' Recipe Tuning
#' 
#' Predictive peformance-based tuning of a preprocessing recipe over a grid of
#' parameters values.
#' 
#' @param x untrained \code{\link[recipes]{recipe}}.
#' @param grid \code{RecipeGrid} containing parameter values at which to
#'   evaluate a recipe, such as those returned by \code{\link{expand_steps}}.
#' @param model \link[=models]{model} function, function name, or call.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Recipe
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe tuning.
#' @param ... arguments passed to the \link{performance} functions.
#' 
#' @return The recipe updated with tuned parameter values.
#' 
#' @seealso \code{\link{fit}}
#' 
#' @examples
#' library(recipes)
#' library(MASS)
#' 
#' rec <- recipe(medv ~ ., data = Boston) %>%
#'   step_pca(all_numeric(), -all_outcomes(), id = "pca")
#' 
#' grid <- expand_steps(
#'   pca = list(num_comp = 1:3)
#' )
#' 
#' tune_recipe(rec, grid = grid, model = GLMModel)
#' 
#' @noRd
#' 
tune_recipe <- function(x, grid, model,
                        control = MachineShop::settings("control"),
                        metrics = NULL,
                        stat = MachineShop::settings("stat.Tune"), ...) {
  
  stopifnot(is(x, "recipe"))
  stopifnot(is(grid, "RecipeGrid"))
  
  if (any(dim(grid) == 0)) return(x)
  
  update_x <- list(update, x)
  models <- list(getMLObject(model, "MLModel"))
  control <- getMLObject(control, "MLControl")
  
  n <- nrow(grid)
  perf_stats <- numeric(n)
  for (i in seq_len(n)) {
    x <- eval(as.call(c(update_x, grid[i, , drop = FALSE])))
    tuned_model <- tune(x, models = models, control = control,
                        metrics = metrics, stat = stat, ...)
    perf_stats[i] <- tuned_model@selected$value
  }
  
  index <- ifelse(tuned_model@metric@maximize, which.max, which.min)(perf_stats)
  eval(as.call(c(update_x, grid[index, , drop = FALSE])))
  
}
