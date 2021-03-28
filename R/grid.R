#' Tuning Grid Control
#'
#' Defines control parameters for a tuning grid.
#'
#' @param size single integer or vector of integers whose positions or names
#'   match the parameters in a model's tuning grid and which specify the number
#'   of values to use in constructing the grid.
#' @param random number of unique grid points to sample at random, \code{Inf}
#'   for all random points, or \code{FALSE} for all fixed points.
#' @param length deprecated argument; use \code{size} instead.
#'
#' @details
#' Returned \code{Grid} objects may be supplied to \code{\link{TunedModel}} for
#' automated construction of model tuning grids.  These grids can be extracted
#' manually and viewed with the \code{\link{expand_modelgrid}} function.
#'
#' @return \code{Grid} class object.
#'
#' @seealso \code{\link{TunedModel}}, \code{\link{expand_modelgrid}}
#'
#' @examples
#' TunedModel(GBMModel, grid = Grid(10, random = 5))
#'
Grid <- function(size = 3, random = FALSE, length = NULL) {
  if (!is.null(length)) {
    depwarn("'length' argument to Grid is deprecated",
            "use 'size' argument instead", expired = Sys.Date() >= "2021-04-15")
    size <- length
  }

  if (length(size) && all(is.finite(size))) {
    storage.mode(size) <- "integer"
    if (any(size < 0)) stop("grid 'size' values must be >= 0")
  } else {
    stop("grid 'size' must contain one or more numeric values")
  }

  if (isTRUE(random) || (length(random) && is.numeric(random))) {
    random <- floor(random[[1]])
    if (random < 1) stop ("number of 'random' grid points must be >= 1")
  } else if (!isFALSE(random)) {
    stop("'random' grid value must be logical or numeric")
  }

  new("Grid", size = size, random = random)
}


#' Tuning Parameters Grid
#'
#' Defines a tuning grid from a set of parameters.
#'
#' @rdname ParameterGrid
#'
#' @param ... named \code{param} objects as defined in the \pkg{dials} package.
#' @param x list of named \code{param} objects or a
#'   \code{\link[dials]{parameters}} object.
#' @param size single integer or vector of integers whose positions or names
#'   match the given parameters and which specify the number of values to use in
#'   constructing a regular grid if \code{random = FALSE}; ignored otherwise.
#' @param random number of unique grid points to sample at random or
#'   \code{FALSE} for all points from a regular grid defined by \code{size}.
#' @param length deprecated argument; use \code{size} instead.
#'
#' @return \code{ParameterGrid} class object that inherits from
#' \code{parameters} and \code{Grid}.
#'
#' @seealso \code{\link{TunedModel}}
#'
#' @examples
#' ## GBMModel tuning parameters
#' grid <- ParameterGrid(
#'   n.trees = dials::trees(),
#'   interaction.depth = dials::tree_depth(),
#'   random = 5
#' )
#' TunedModel(GBMModel, grid = grid)
#'
ParameterGrid <- function(...) {
  UseMethod("ParameterGrid")
}


#' @rdname ParameterGrid
#'
ParameterGrid.param <- function(..., size = 3, random = FALSE, length = NULL) {
  ParameterGrid(list(...), size = size, random = random, length = length)
}


#' @rdname ParameterGrid
#'
ParameterGrid.list <- function(
  x, size = 3, random = FALSE, length = NULL, ...
) {
  ParameterGrid(parameters(x), size = size, random = random, length = length)
}


#' @rdname ParameterGrid
#'
ParameterGrid.parameters <- function(
  x, size = 3, random = FALSE, length = NULL, ...
) {
  if (!is.null(length)) {
    depwarn("'length' argument to ParameterGrid is deprecated",
            "use 'size' argument instead", expired = Sys.Date() >= "2021-04-15")
    size <- length
  }

  if (length(size) && all(is.finite(size))) {
    storage.mode(size) <- "integer"
    if (any(size < 0)) stop("grid 'size' values must be >= 0")
  } else {
    stop("grid 'size' must contain one or more numeric values")
  }

  if (isFALSE(random)) {
    if (!is.null(names(size))) {
      if (!all(names(size) %in% x$id)) {
        warn("Unmatched parameter names in ParameterGrid() argument 'size'.\n",
             "x Existing data has ", label_items("parameter", x$id), ".\n",
             "x Assigned data has ", label_items("name", names(size)), ".")
      }
      size <- size[x$id]
      size[is.na(size)] <- 0L
    } else if (length(size) > 1 && length(size) != nrow(x)) {
      stop("Length of ParameterGrid() argument 'size' must equal 1",
           " or the number of parameters.\n",
           "x Existing data has ", nrow(x), " ",
           label_items("parameter", x$id), ".\n",
           "x Assigned data has ", length(size), " ",
           label_items("size", size), ".",
           call. = FALSE)
    }
    keep <- size >= 1
    x <- x[keep, ]
    size <- size[keep]
  } else if (length(random) && is.finite(random)) {
    random <- as.integer(random[[1]])
    if (random < 1) stop ("number of 'random' grid points must be >= 1")
  } else {
    stop("'random' grid value must be logical or numeric")
  }

  new("ParameterGrid", x, size = size, random = random)
}


new_gridinfo <- function(param = character(), values = list(), regular = NULL) {
  if (is.null(regular)) regular <- TRUE

  stopifnot(is.character(param))
  stopifnot(is.list(values))
  stopifnot(is.logical(regular))

  if (!all(map_logi(is.function, values))) {
    stop("'values' must be a list of functions")
  }

  as_tibble(list(param = param, values = values, regular = regular))
}
