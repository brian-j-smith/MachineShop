#' Tuning Grid Control
#'
#' Defines control parameters for a tuning grid.
#'
#' @param size single integer or vector of integers whose positions or names
#'   match the parameters in a model's tuning grid and which specify the number
#'   of values used to construct the grid.
#' @param random number of unique points to sample at random from the grid
#'   defined by \code{size}.  If \code{size} is a single unnamed integer, then
#'   \code{random = Inf} will include all values of all grid parameters in the
#'   constructed grid, whereas \code{random = FALSE} will include all values of
#'   default grid parameters.
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
Grid <- function(size = 3, random = FALSE) {
  if (length(size) && all(is.finite(size))) {
    storage.mode(size) <- "integer"
    if (any(size < 0)) {
      size <- Error("Value must be one or more numerics >= 0.")
      throw(check_assignment(size))
    }
  } else {
    size <- Error("Value must be one or more numerics.")
    throw(check_assignment(size))
  }

  if (isTRUE(random) || (length(random) && is.numeric(random))) {
    random <- floor(random[[1]])
    if (random < 1) {
      random <- Error("Value must be >= 1.")
      throw(check_assignment(random))
    }
  } else if (!isFALSE(random)) {
    random <- TypeError(random, c("logical", "numeric"), "value")
    throw(check_assignment(random))
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
#' @param object list of named \code{param} objects or a
#'   \code{\link[dials]{parameters}} object.  This is a positional argument
#'   that must be given first in calls to its methods.
#' @param size single integer or vector of integers whose positions or names
#'   match the given parameters and which specify the number of values used to
#'   construct the grid.
#' @param random number of unique points to sample at random from the grid
#'   defined by \code{size}, or \code{FALSE} for all points.
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
ParameterGrid.param <- function(..., size = 3, random = FALSE) {
  ParameterGrid(list(...), size = size, random = random)
}


#' @rdname ParameterGrid
#'
ParameterGrid.list <- function(object, size = 3, random = FALSE, ...) {
  ParameterGrid(parameters(object), size = size, random = random)
}


#' @rdname ParameterGrid
#'
ParameterGrid.parameters <- function(object, size = 3, random = FALSE, ...) {
  grid <- Grid(size = size, random = random)

  size <- grid@size
  if (!is.null(names(size))) {
    if (!all(names(size) %in% object$id)) {
      throw(LocalWarning(
        "Unmatched parameter names in ParameterGrid() argument 'size'.\n",
        "x Existing data has ", label_items("parameter", object$id), ".\n",
        "x Assigned data has ", label_items("name", names(size)), "."
      ))
    }
    size <- size[object$id]
    size[is.na(size)] <- 0L
  } else if (length(size) > 1 && length(size) != nrow(object)) {
    throw(LocalError(
      "Length of ParameterGrid() argument 'size' must equal 1 ",
      "or the number of parameters.\n",
      "x Existing data has ", nrow(object), " ",
      label_items("parameter", object$id), ".\n",
      "x Assigned data has ", length(size), " ", label_items("size", size), "."
    ))
  }
  keep <- size >= 1
  object <- object[keep, ]
  size <- size[keep]

  new("ParameterGrid", object, size = size, random = grid@random)
}


new_gridinfo <- function(
  param = character(), get_values = list(), default = NULL
) {
  if (is.null(default)) default <- TRUE

  stopifnot(is.character(param))
  stopifnot(is.list(get_values))
  stopifnot(is.logical(default))

  if (!all(map_logi(is.function, get_values))) {
    get_values <- Error("Value must be a list of functions.")
    throw(check_assignment(get_values))
  }

  as_tibble(list(param = param, get_values = get_values, default = default))
}
