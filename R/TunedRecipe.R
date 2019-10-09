#' Tuned Recipe
#' 
#' Recipe tuned over a grid of parameter values.
#' 
#' @param x untrained \code{\link[recipes]{recipe}}.
#' @param grid \code{RecipeGrid} containing parameter values at which to
#'   evaluate a recipe, such as those returned by \code{\link{expand_steps}}.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Recipe
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe tuning.
#' @param cutoff argument passed to the \code{metrics} functions.
#' 
#' @return \code{TunedRecipe} class object that inherits from \code{recipe}.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
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
#' fit(TunedRecipe(rec, grid = grid), model = GLMModel)
#' 
TunedRecipe <- function(x, grid = expand_steps(),
                        control = MachineShop::settings("control"),
                        metrics = NULL,
                        stat = MachineShop::settings("stat.Tune"),
                        cutoff = MachineShop::settings("cutoff")) {
  
  obj <- new("TunedRecipe", ModelRecipe(x), grid = grid,
             params = list(control = control, metrics = metrics, stat = stat,
                           cutoff = cutoff))
  
  grid_names <- names(obj@grid)
  step_ids <- sapply(obj$steps, getElement, name = "id")
  found <- grid_names %in% step_ids
  if (!all(found)) {
    stop("grid step names ", toString(paste0("'", grid_names[!found], "'")),
         " not found in recipe step ids ", toString(paste0("'", step_ids, "'")))
  }
  
  obj
  
}


setAs("TunedRecipe", "list",
  function(from) c(list(grid = from@grid), from@params)
)


setAs("TunedRecipe", "ModelRecipe",
  function(from) structure(asS3(from), grid = NULL, params = NULL)
)


setAs("TunedRecipe", "recipe",
  function(from) as(from, "ModelRecipe")
)
