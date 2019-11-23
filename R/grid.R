#' Tuning Grid Control
#' 
#' Defines control parameters for a tuning grid.
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
#' @examples
#' TunedModel(GBMModel, grid = Grid(10, random = 5))
#' 
Grid <- function(length = 3, random = FALSE) {
  if (is.finite(length)) {
    length <- as.integer(length[[1]])
    if (length <= 0) stop("grid parameter 'length' must be >= 1")
  } else {
    stop("grid parameter 'length' must be numeric")
  }
  
  if (isTRUE(random) || is.numeric(random)) {
    random <- floor(random[[1]])
    if (random <= 0) stop ("number of 'random' grid points must be >= 1")
  } else if (!isFALSE(random)) {
    stop("'random' grid value must be logical or numeric")
  }
  
  new("Grid", length = length, random = random)
}


#' Tuning Parameter Set
#' 
#' Defines a tuning grid from a parameter set.
#' 
#' @rdname ParamSet
#' 
#' @param ... \code{\link[dials]{param_set}} object, named \code{param} objects
#'   as defined in the \pkg{dials} package, or a list of these.
#' @param length single number or vector of numbers of parameter values to use
#'   in constructing a regular grid if \code{random = FALSE}; ignored otherwise.
#' @param random number of unique grid points to sample at random or
#'   \code{FALSE} for all points from a regular grid defined by \code{length}.
#' 
#' @return \code{ParamSet} class object that inherits from \code{param_set} and
#' \code{Grid}.
#' 
#' @seealso \code{\link{TunedModel}}
#' 
#' @examples
#' ## GBMModel tuning parameters
#' library(dials)
#' 
#' grid <- ParamSet(
#'   n.trees = trees(),
#'   interaction.depth = tree_depth(),
#'   random = 5
#' )
#' TunedModel(GBMModel, grid = grid)
#' 
ParamSet <- function(..., length = 3, random = FALSE) {
  x <- list(...)
  if (is_one_element(x, "list") && is.null(names(x))) x <- x[[1]]
  .ParamSet(x, length = length, random = random)
}


.ParamSet <- function(x, ...) {
  UseMethod(".ParamSet")
}


.ParamSet.list <- function(x, ...) {
  .ParamSet(param_set(x), ...)
}


.ParamSet.param_set <- function(x, length, random, ...) {
  if (all(is.finite(length))) {
    length <- as.integer(length)
    if (any(length < 0)) stop("grid parameter 'length' must be >= 0")
  } else {
    stop("grid parameter 'length' must be numeric")
  }
  
  if (isFALSE(random)) {
    if (length(length) > 1) length <- rep(length, length.out = nrow(x))
    keep <- length > 0
    x <- x[keep, ]
    length <- length[keep]
  } else if (is.finite(random)) {
    random <- as.integer(random[[1]])
    if (random <= 0) stop ("number of 'random' grid points must be >= 1")
  } else {
    stop("'random' grid value must be logical or numeric")
  }
  
  new("ParamSet", x, length = length, random = random)
}


as.grid <- function(x, ...) {
  UseMethod("as.grid")
}


as.grid.default <- function(x, ...) {
  stop("unsupported grid object of class ", class(x)[1])
}


as.grid.tbl_df <- function(x, fixed = tibble(), ...) {
  x[names(fixed)] <- fixed
  if (ncol(x)) x[!duplicated(x), ] else x
}


as.grid.Grid <- function(x, ..., model, fixed = tibble()) {
  mf <- ModelFrame(..., na.rm = FALSE)
  params_list <- model@grid(mf, length = x@length, random = x@random)
  params <- lapply(params_list, unique)
  params[sapply(params, length) == 0] <- NULL
  as.grid(expand_params(params, random = x@random), fixed = fixed)
}


as.grid.ParamSet <- function(x, ..., model, fixed = tibble()) {
  grid <- if (nrow(x)) {
    if (any(sapply(x$object, dials::has_unknowns))) {
      mf <- ModelFrame(..., na.rm = FALSE)
      data <- switch(model@predictor_encoding,
                     "model.matrix" = model.matrix(mf, intercept = FALSE),
                     "terms" = {
                       mf_terms <- attributes(terms(mf))
                       var_list <- eval(mf_terms$variables, mf)
                       names(var_list) <- rownames(mf_terms$factors)
                       as.data.frame(var_list[-c(1, mf_terms$offset)])
                     })
      x <- dials::finalize(x, x = data)
    }
    if (x@random) {
      dials::grid_random(x, size = x@random)
    } else {
      dials::grid_regular(x, levels = x@length)
    }
  } else {
    tibble(.rows = 1)
  }
  as.grid(grid, fixed = fixed)
}
