#' Model Expansion Over a Grid of Tuning Parameters
#'
#' Expand a model over all combinations of a grid of tuning parameters.
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
  expand.model(list(get(x@name, mode = "function"), grid))
}
