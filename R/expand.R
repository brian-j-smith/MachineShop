#' Model Expansion Over a Grid of Tuning Parameters
#'
#' Expand a model over all combinations of a grid of tuning parameters.
#' 
#' @param x \link[=models]{model} function, function name, or call.
#' @param ... vectors, factors, or list containing the parameter values over
#' which to expand \code{x}.
#' 
#' @return A list of model objects created from the parameter combinations.
#' 
#' @seealso \code{\link{tune}}
#' 
#' @examples
#' expand.model(GBMModel, n.trees = c(25, 50, 100),
#'                        interaction.depth = 1:3,
#'                        n.minobsinnode = c(5, 10))
#' 
expand.model <- function(x, ...) {
  .expand.model(x, ...)
}


.expand.model <- function(x, ...) {
  UseMethod(".expand.model")
}


.expand.model.default <- function(x, ...) {
  expand.model(getMLObject(x, class = "MLModel"), ...)
}


.expand.model.list <- function(x, ...) {
  grid <- x[[2]]
  models <- split(grid, seq(max(1, nrow(grid)))) %>%
    lapply(function(args) do.call(x[[1]], args))
  structure(models, names = paste0(models[[1]]@name, ".", names(models)))
}


.expand.model.MLModel <- function(x, ...) {
  grid <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  expand.model(list(fget(x@name), grid))
}


#' Recipe Step Parameters Expansion
#'
#' Create a grid of parameter values from all combinations of lists supplied for
#' steps of a preprocessing recipe.
#' 
#' @param ... one or more lists containing parameter values over which to create
#' the grid.  For each list an argument name should be given as the \code{id} of
#' the \link[recipes]{recipe} step to which it corresponds.
#' 
#' @return \code{RecipeGrid} class object that inherits from \code{data.frame}.
#' 
#' @seealso \code{\link{TunedRecipe}}
#' 
#' @examples
#' library(recipes)
#' library(MASS)
#' 
#' rec <- recipe(medv ~ ., data = Boston) %>%
#'   step_corr(all_numeric(), -all_outcomes(), id = "corr") %>%
#'   step_pca(all_numeric(), -all_outcomes(), id = "pca")
#'
#' expand_steps(
#'   corr = list(threshold = c(0.8, 0.9),
#'               method = c("pearson", "spearman")),
#'   pca = list(num_comp = 1:3)
#' )
#' 
expand_steps <- function(...) {
  
  steps <- list(...)
  step_names <- names(steps)
  if(length(steps) == 1 && is.null(step_names) && is.list(steps[[1]])) {
    steps <- steps[[1]]
    step_names <- names(steps)
  }
  
  if (!all(sapply(steps, is.list))) stop("step arguments must be lists")
  
  get_names <- function(x) {
    res <- NULL
    if (is.list(x)) {
      for (i in seq(x)) {
        name <- names(x[i])
        if (is.null(name)) name <- ""
        res <- c(res, name, get_names(x[[i]]))
      }
    }
    res
  }

  if (!all(nzchar(get_names(steps)))) {
    stop("all steps and their parameters must be named")
  } else if (any(duplicated(step_names))) {
    stop("step names must be unique")
  }
  
  grid <- expand.grid(unlist(steps, recursive = FALSE),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  
  recipe_grid <- data.frame(row.names = seq_len(nrow(grid)))
  
  offset <- 0
  for (name in step_names) {
    indices <- offset + seq_len(length(steps[[name]]))
    x <- grid[indices]
    names(x) <- substring(names(x), nchar(name) + 2)
    recipe_grid[[name]] <- x
    offset <- offset + length(indices)
  }
  
  RecipeGrid(recipe_grid)
  
}
