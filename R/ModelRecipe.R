ModelRecipe <- function(object, ...) {
  UseMethod("ModelRecipe")
}


ModelRecipe.ModelRecipe <- function(object, ...) {
  object
}


ModelRecipe.recipe <- function(object, ...) {
  if (any(sapply(object$steps, function(step) isTRUE(step$trained)))) {
    stop("recipe must be untrained")
  }
  structure(object, class = c("ModelRecipe", "recipe"))
}


as.data.frame.ModelRecipe <- function(x, ...) {
  as.data.frame(x$template)
}


juice <- function(x, ...) {
  UseMethod("juice")
}


juice.default <- function(x, ...) {
  recipes::juice(x, ...)
}


juice.ModelRecipe <- function(x, ...) {
  bake(x, x$template)
}


prep.ModelRecipe <- function(x, ...) {
  if (!fully_trained(x)) {
    x_class <- class(x)
    class(x) <- "recipe"
    structure(prep(x, retain = FALSE), class = x_class)
  } else x
}


recipe.ModelRecipe <- function(x, data, ...) {
  if (fully_trained(x)) {
    x <- prep(x, training = data, fresh = TRUE, retain = FALSE)
  }
  x$template <- as_tibble(data)
  x
}
