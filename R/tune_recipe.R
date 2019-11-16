#' Recipe Tuning and Selection
#' 
#' Predictive performance-based tuning of a preprocessing recipe or selection
#' from a candidate set.
#' 
#' @rdname tune_recipe
#' 
#' @param x \code{\link{SelectedRecipe}} or \code{\link{TunedRecipe}} object.
#' @param model \link[=models]{model} function or call.
#' @param ... arguments passed to other methods.
#' 
#' @return List containing the following components:
#' \describe{
#'   \item{recipe}{tuned \code{ModelRecipe} object.}
#'   \item{model}{tuned \code{MLModel} object.}
#' }
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#' 
#' @noRd
#' 
tune_recipe <- function(x, model, ...) {
  UseMethod("tune_recipe")
}


tune_recipe.recipe <- function(x, model, ...) {
  list(recipe = prep(ModelRecipe(x)), model = model)
}


#' @rdname tune_recipe
#' 
tune_recipe.SelectedRecipe <- function(x, model, ...) {
  
  last_tune <- model@tune
  if (is(model, "SelectedModel") || is(model, "TunedModel")) {
    model@params[names(x@params)] <- x@params
  } else {
    model <- do.call(SelectedModel, c(model, x@params))
  }
  recipes <- x@recipes
  
  data <- as.data.frame(x)
  setdata <- function(x) recipe(x, data[unique(summary(x)$variable)])
  
  tuned_models <- list()
  num_models <- integer()
  for (i in seq(recipes)) {
    rec <- setdata(recipes[[i]])
    tuned_model <- as.MLModel(fit(model, rec))
    num_models[i] <- nrow(tuned_model@tune@grid)
    tuned_model@tune@grid <- tibble(
      Model = tibble(Index = seq_len(num_models[i])),
      Recipe = tibble(Index = rep(i, num_models[i]))
    )
    tuned_models[[i]] <- tuned_model
  }
  
  tune <- do.call(c, lapply(tuned_models, slot, name = "tune"))
  selected <- max(which(tune@selected > c(0, cumsum(num_models))))
  recipe <- setdata(recipes[[selected]])
  model <- tuned_models[[selected]]
  model@tune <- if (is.null(last_tune)) tune else last_tune
  
  list(recipe = recipe, model = model)
  
}


#' @rdname tune_recipe
#' 
tune_recipe.TunedRecipe <- function(x, model, ...) {
  
  last_tune <- model@tune
  if (is(model, "SelectedModel") || is(model, "TunedModel")) {
    model@params[names(x@params)] <- x@params
  } else {
    model <- do.call(SelectedModel, c(model, x@params))
  }
  grid <- x@grid
  x <- as(x, "ModelRecipe")
  
  if (any(dim(grid) == 0)) return(x)
  
  update_x <- list(update, x)
  
  tuned_models <- list()
  grid_inds <- seq_len(nrow(grid))
  for (i in grid_inds) {
    x <- eval(as.call(c(update_x, grid[i, ])))
    tuned_models[[i]] <- tune_model(model, x)
  }
  
  tune <- do.call(c, lapply(tuned_models, slot, name = "tune"))
  num_models <- nrow(tune@grid) / nrow(grid)
  tune@grid <- tibble(
    Model = tune@grid,
    Recipe = grid[rep(grid_inds, each = num_models), ]
  )
  
  selected <- ceiling(tune@selected / num_models)
  recipe <- eval(as.call(c(update_x, grid[selected, ])))
  model <- tuned_models[[selected]]
  model@tune <- if (is.null(last_tune)) tune else last_tune
  
  list(recipe = recipe, model = model)
  
}
