#' Selected Recipe
#'
#' Recipe selection from a candidate set.
#'
#' @param ... untrained \code{\link[recipes:recipe]{recipes}} or a list of them.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Recipe
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe selection.
#' @param cutoff argument passed to the \code{metrics} functions.
#'
#' @return \code{SelectedRecipe} class object that inherits from \code{recipe}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' library(recipes)
#' library(MASS)
#'
#' rec1 <- recipe(medv ~ crim + zn + indus + chas + nox + rm, data = Boston)
#' rec2 <- recipe(medv ~ chas + nox + rm + age + dis + rad + tax, data = Boston)
#'
#' fit(SelectedRecipe(rec1, rec2), model = GLMModel)
#'
SelectedRecipe <- function(..., control = MachineShop::settings("control"),
                           metrics = NULL,
                           stat = MachineShop::settings("stat.train"),
                           cutoff = MachineShop::settings("cutoff")) {

  recipes <- list(...)
  if (is_one_element(recipes, "list") && !is(recipes, "recipe")) {
    recipes <- recipes[[1]]
  }
  names(recipes) <- make_list_names(recipes, "Recipe")

  for (i in seq(recipes)) recipes[[i]] <- ModelRecipe(recipes[[i]])

  getinfo <- function(x) {
    info <- summary(x)
    info <- info[info$role != "predictor", ]
    info_order <- do.call(order, info)
    info[info_order, ]
  }

  info <- getinfo(recipes[[1]])
  same_info <- sapply(recipes[-1], function(x) {
    identical(info, getinfo(x))
  })

  if (!all(same_info)) stop("non-predictor variables in recipes differ")

  data <- NULL
  for (i in seq(recipes)) {
    data <- combine_dataframes(as.data.frame(recipes[[i]]), data)
    recipes[[i]] <- recipe(recipes[[i]], tibble())
  }

  outcome_vars <- info$variable[info$role == "outcome"]
  fo <- reformulate(".", paste(outcome_vars, collapse = "+"))
  new("SelectedRecipe", new("ModelRecipe", recipe(fo, data = data)),
      recipes = recipes,
      params = list(control = getMLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff))

}


.fit.SelectedRecipe <- function(x, model, ...) {
  recipes <- x@recipes
  data <- as.data.frame(x)
  set_data <- function(x) recipe(x, data[unique(summary(x)$variable)])
  trainbits <- resample_selection(recipes, set_data, x@params, model)
  trainbits$grid <- tibble(Recipe = tibble(Index = seq(recipes)))
  recipe <- set_data(recipes[[trainbits$selected]])
  push(do.call(TrainBits, trainbits), fit(recipe, model = model))
}


#' Tuned Recipe
#'
#' Recipe tuning over a grid of parameter values.
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
#' @seealso \code{\link{fit}}, \code{\link{resample}}
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
                        stat = MachineShop::settings("stat.train"),
                        cutoff = MachineShop::settings("cutoff")) {

  object <- new("TunedRecipe", ModelRecipe(x),
                grid = grid,
                params = list(control = getMLObject(control, "MLControl"),
                              metrics = metrics, stat = stat, cutoff = cutoff))

  grid_names <- names(object@grid)
  step_ids <- sapply(object$steps, getElement, name = "id")
  found <- grid_names %in% step_ids
  if (!all(found)) {
    missing <- grid_names[!found]
    stop(label_items("grid step name", missing),
         label_items("; not found in recipe step id", step_ids))
  }

  object

}


.fit.TunedRecipe <- function(x, model, ...) {
  grid <- x@grid
  recipe <- as(x, "ModelRecipe")
  if (all(dim(grid) != 0)) {
    grid_split <- split(grid, 1:nrow(grid))
    set_steps <- function(x) update(recipe, x)
    trainbits <- resample_selection(grid_split, set_steps, x@params, model)
    trainbits$grid <- tibble(Recipe = grid)
    recipe <- set_steps(grid_split[[trainbits$selected]])
    push(do.call(TrainBits, trainbits), fit(recipe, model = model))
  } else {
    fit(recipe, model = model)
  }
}
