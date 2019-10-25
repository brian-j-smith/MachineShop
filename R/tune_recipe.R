#' Recipe Tuning and Selection
#' 
#' Predictive peformance-based tuning of a preprocessing recipe or selection
#' from a candidate set.
#' 
#' @rdname tune_recipe
#' 
#' @param x,model,recipe \code{\link{SelectedRecipe}} or
#'   \code{\link{TunedRecipe}} object or \link[=models]{model} function,
#'   function name, or call.
#' @param ... arguments passed to other methods.
#' 
#' @return Tuned \code{ModelRecipe} class object that inherits from
#' \code{recipe}.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @noRd
#' 
tune_recipe <- function(x, ...) {
  UseMethod("tune_recipe")
}


tune_recipe.recipe <- function(x, ...) {
  ModelRecipe(x)
}


#' @rdname tune_recipe
#' 
tune_recipe.SelectedRecipe <- function(x, model, ...) {
  
  recipes <- x@recipes
  params <- x@params
  x <- as(x, "ModelRecipe")
  
  models <- list(getMLObject(model, "MLModel"))
  
  data <- as.data.frame(x)
  setdata <- function(x) recipe(x, data[unique(summary(x)$variable)])
  
  n <- length(recipes)
  perf_stats <- numeric(n)
  for (i in seq_len(n)) {
    rec <- setdata(recipes[[i]])
    tuned_model <- tune(rec, models = models, control = params$control,
                        metrics = params$metrics, stat = params$stat,
                        cutoff = params$cutoff)
    perf_stats[i] <- tuned_model@selected$value
  }
  
  index <- ifelse(tuned_model@metric@maximize, which.max, which.min)(perf_stats)
  setdata(recipes[[index]])
  
}


#' @rdname tune_recipe
#' 
tune_recipe.TunedRecipe <- function(x, model, ...) {
  
  grid <- x@grid
  params <- x@params
  x <- as(x, "ModelRecipe")
  
  if (any(dim(grid) == 0)) return(x)
  
  update_x <- list(update, x)
  models <- list(getMLObject(model, "MLModel"))
  
  n <- nrow(grid)
  perf_stats <- numeric(n)
  for (i in seq_len(n)) {
    x <- eval(as.call(c(update_x, grid[i, , drop = FALSE])))
    tuned_model <- tune(x, models = models, control = params$control,
                        metrics = params$metrics, stat = params$stat,
                        cutoff = params$cutoff)
    perf_stats[i] <- tuned_model@selected$value
  }
  
  index <- ifelse(tuned_model@metric@maximize, which.max, which.min)(perf_stats)
  eval(as.call(c(update_x, grid[index, , drop = FALSE])))
  
}


#' @rdname tune_recipe
#' 
tune_recipe.MLModel <- function(x, recipe, ...) {
  tune_recipe(recipe, model = x)
}


#' @rdname tune_recipe
#' 
tune_recipe.MLModelFunction <- function(x, recipe, ...) {
  tune_recipe(recipe, model = x)
}
