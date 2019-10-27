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
  
  model <- do.call(SelectedModel, c(getMLObject(model, "MLModel"), x@params))
  recipes <- x@recipes
  x <- as(x, "ModelRecipe")
  
  data <- as.data.frame(x)
  setdata <- function(x) recipe(x, data[unique(summary(x)$variable)])
  
  n <- length(recipes)
  perf_stats <- numeric(n)
  for (i in seq_len(n)) {
    rec <- setdata(recipes[[i]])
    tune <- tune(model, rec)@tune
    perf_stats[i] <- tune@values[tune@selected]
  }
  
  selected <- ifelse(tune@metric@maximize, which.max, which.min)(perf_stats)
  setdata(recipes[[selected]])
  
}


#' @rdname tune_recipe
#' 
tune_recipe.TunedRecipe <- function(x, model, ...) {
  
  model <- do.call(SelectedModel, c(getMLObject(model, "MLModel"), x@params))
  grid <- x@grid
  x <- as(x, "ModelRecipe")
  
  if (any(dim(grid) == 0)) return(x)
  
  update_x <- list(update, x)
  
  n <- nrow(grid)
  perf_stats <- numeric(n)
  for (i in seq_len(n)) {
    x <- eval(as.call(c(update_x, grid[i, ])))
    tune <- tune(model, x)@tune
    perf_stats[i] <- tune@values[tune@selected]
  }
  
  selected <- ifelse(tune@metric@maximize, which.max, which.min)(perf_stats)
  eval(as.call(c(update_x, grid[selected, ])))
  
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
