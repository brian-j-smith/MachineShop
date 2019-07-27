#' Model Expansion Over a Grid of Tuning Parameters
#'
#' Expand a model over all combinations of a grid of tuning parameters.
#' 
#' @rdname expand_model
#' 
#' @param x \code{MLModel} function, function name, or object.
#' @param ... vectors, factors, or a list containing the parameter values.
#' 
#' @return A list of \code{MLModel} objects created from the parameter
#' combinations.
#' 
#' @seealso \code{\link{modelinfo}}, \code{\link{tune}}
#' 
#' @examples
#' expand_model(GBMModel, n.trees = c(25, 50, 100),
#'                        interaction.depth = 1:3,
#'                        n.minobsinnode = c(5, 10))
#' 
expand_model <- function(x, ...) {
  .expand_model(x, ...)
}


#' @rdname expand_model
#' 
expand.model <- function(...) {
  depwarn("'expand.model' function is deprecated",
          "use 'expand_model' instead",
          expired = Sys.Date() >= "2019-09-01")
  expand_model(...)
}


.expand_model <- function(x, ...) {
  UseMethod(".expand_model")
}


.expand_model.default <- function(x, ...) {
  expand_model(getMLObject(x, class = "MLModel"), ...)
}


.expand_model.list <- function(x, ...) {
  grid <- x[[2]]
  models <- split(grid, seq(max(1, nrow(grid)))) %>%
    lapply(function(args) do.call(x[[1]], args))
  structure(models, names = paste0(models[[1]]@name, ".", names(models)))
}


.expand_model.MLModel <- function(x, ...) {
  grid <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  expand_model(list(get(x@name, mode = "function"), grid))
}
