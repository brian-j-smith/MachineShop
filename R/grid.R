grid <- function(x, ...) {
  UseMethod("grid")
}


grid.formula <- function(x, data, model, length = 3, ...) {
  grid(ModelFrame(x, data, na.action = na.pass), model, length = length)
}


grid.ModelFrame <- function(x, model, length = 3, ...) {
  model <- getMLObject(model, "MLModel")
  params <- model@grid(x, length = max(as.integer(length), 1L)) %>%
    lapply(unique)
  params[sapply(params, length) == 0] <- NULL
  expand.grid(params, stringsAsFactors = FALSE)
}


grid.recipe <- function(x, model, length = 3, ...) {
  grid(ModelFrame(x, na.action = na.pass), model, length = length)
}
