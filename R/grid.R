#' Tuning Grid Control
#' 
#' Defines the control parameters for a tuning grid.
#' 
#' @param length number of values to be generated for each model parameter in
#' the tuning grid.
#' @param random number of points to be randomly sampled from the tuning grid or
#' \code{FALSE} if all points are to be used.
#' 
#' @seealso \code{\link{tune}}
#' 
Grid <- function(length = 3, random = FALSE) {
  structure(
    list(length = length, random = random),
    class = "Grid"
  )
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
  if (random) {
    grid <- as.data.frame(lapply(params, sample, size = random, replace = TRUE),
                          stringsAsFactors = FALSE)
    grid[!duplicated(grid), , drop = FALSE]
  } else {
    expand.grid(params, stringsAsFactors = FALSE)
  }
}


grid.recipe <- function(x, ...) {
  grid(ModelFrame(x, na.rm = FALSE), ...)
}
