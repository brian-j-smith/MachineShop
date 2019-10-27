#' Tuning Grid Control
#' 
#' Defines the control parameters for a tuning grid.
#' 
#' @param length number of values to be generated for each model parameter in
#'   the tuning grid.
#' @param random number of unique grid points to sample at random, \code{Inf}
#'   for all random points, or \code{FALSE} for all fixed points.
#' 
#' @return \code{Grid} class object.
#' 
#' @seealso \code{\link{TunedModel}}
#' 
Grid <- function(length = 3, random = FALSE) {
  if (is(length, "numeric")) {
    length <- as.integer(length[[1]])
    if (length <= 0) stop("grid parameter 'length' must be >= 1")
  } else {
    stop ("grid parameter 'length' must be numeric")
  }
  
  if (isTRUE(random) || is(random, "numeric")) {
    random <- floor(random[[1]])
    if (random <= 0) stop ("number of 'random' grid points must be >= 1")
  } else if (!isFALSE(random)) {
    stop("'random' grid value must be logical or numeric")
  }
  
  new("Grid", length = length, random = random)
}


grid <- function(x, ...) {
  UseMethod("grid")
}


grid.formula <- function(x, data, ...) {
  grid(ModelFrame(x, data, na.rm = FALSE), ...)
}


grid.matrix <- function(x, y, ...) {
  grid(ModelFrame(x, y, na.rm = FALSE), ...)
}


grid.ModelFrame <- function(x, model, length = 3, random = FALSE, ...) {
  model <- getMLObject(model, "MLModel")
  length <- max(as.integer(length), 1L)
  params <- lapply(model@grid(x, length = length, random = random), unique)
  params[sapply(params, length) == 0] <- NULL
  expand_params(params, random = random)
}


grid.recipe <- function(x, ...) {
  grid(ModelFrame(x, na.rm = FALSE), ...)
}
