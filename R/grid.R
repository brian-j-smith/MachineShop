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


as.grid <- function(x, ...) {
  UseMethod("as.grid")
}


as.grid.default <- function(x, ...) {
  stop("unsupported grid object of class ", class(x)[1])
}


as.grid.tbl_df <- function(x, fixed = tibble(), ...) {
  x[names(fixed)] <- fixed
  x[!duplicated(x), ]
}


as.grid.Grid <- function(x, ..., model, fixed = tibble()) {
  mf <- ModelFrame(..., na.rm = FALSE)
  model <- getMLObject(model, "MLModel")
  params_list <- model@grid(mf, length = x@length, random = x@random)
  params <- lapply(params_list, unique)
  params[sapply(params, length) == 0] <- NULL
  as.grid(expand_params(params, random = x@random), fixed = fixed)
}
