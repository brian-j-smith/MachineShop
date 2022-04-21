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
#' \code{parameters} and \code{TuningGrid}.
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
  grid <- TuningGrid(size = size, random = random)

  size <- grid@size
  if (!is.null(names(size))) {
    if (!all(names(size) %in% object$id)) {
      throw(LocalWarning(
        "Unmatched parameter names in ParameterGrid() argument `size`.\n",
        "x Existing data has ", note_items("parameter{?s}: ", object$id), ".\n",
        "x Assigned data has ", note_items("name{?s}: ", names(size)), "."
      ))
    }
    size <- size[object$id]
    size[is.na(size)] <- 0L
  } else if (length(size) > 1 && length(size) != nrow(object)) {
    throw(LocalError(
      "Length of ParameterGrid() argument `size` must equal 1 ",
      "or the number of parameters.\n",
      "x Existing data has ", nrow(object), " ",
      note_items("parameter{?s}: ", object$id), ".\n",
      "x Assigned data has ", length(size), " ", note_items("size{?s}: ", size),
      "."
    ))
  }
  keep <- size >= 1
  object <- object[keep, ]
  size <- size[keep]

  new("ParameterGrid", object, size = size, random = grid@random)
}


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
#' Returned \code{TuningGrid} objects may be supplied to
#' \code{\link{TunedModel}} for automated construction of model tuning grids.
#' These grids can be extracted manually and viewed with the
#' \code{\link{expand_modelgrid}} function.
#'
#' @return \code{TuningGrid} class object.
#'
#' @seealso \code{\link{TunedModel}}, \code{\link{expand_modelgrid}}
#'
#' @examples
#' TunedModel(XGBTreeModel, grid = TuningGrid(10, random = 5))
#'
TuningGrid <- function(size = 3, random = FALSE) {
  size <- check_integer(size, bounds = c(0, Inf), size = NA)
  throw(check_assignment(size))

  if (!isFALSE(random)) {
    random <- check_integer(random, bounds = c(1, Inf), size = 1)
    throw(check_assignment(random))
  }

  new("TuningGrid", size = size, random = random)
}


get_grid <- function(object, ...) {
  UseMethod("get_grid")
}


get_grid.default <- function(object, ...) {
  make_grid(object, class1(object))
}


get_grid.EnsembleInputOrModel <- function(object, ...) {
  make_grid(
    object,
    names(object@candidates),
    tibble(id = map("char", slot, object@candidates, "id"))
  )
}


get_grid.ModelSpecification <- function(object, ...) {
  make_grid(object, "ModelSpec", object@grid)
}


get_grid.StackedModel <- function(object, ...) {
  get_grid.default(object, ...)
}


get_grid.TunedModelRecipe <- function(object, ...) {
  make_grid(object, "ModelRecipe", as(object@grid, "tbl_df"))
}


get_grid.TunedModel <- function(object, ...) {
  make_grid(object, object@model@name, expand_modelgrid(object, ...))
}


grid_diff <- function(x, params = NULL, drop = FALSE) {
  res <- x
  for (name in names(x)) {
    node <- x[[name]]
    if (is_tibble(node)) {
      res[[name]] <- grid_diff(node, params[[name]], drop = TRUE)
    } else if (name %in% names(params)) {
      res[[name]] <- NULL
    }
  }
  if (length(res) || !drop) res
}


make_grid <- function(object, name = character(), params = tibble()) {
  if (is_empty(name)) name <- "Model"
  if (is_empty(params)) params <- tibble(.rows = 1)
  name <- make_unique(rep_len(name, nrow(params)))
  if (is_optim_method(object, "RandomGridSearch")) {
    n <- nrow(params)
    size <- get_optim_field(object, "size")
    inds <- sort(sample.int(n, min(max(size, 1), n)))
    params <- params[inds, ]
    name <- name[inds]
  }
  tibble(name = name, params = params)
}


new_gridinfo <- function(
  param = character(), get_values = list(), default = logical()
) {
  if (is_empty(default)) default <- TRUE

  stopifnot(is.character(param))
  stopifnot(is.list(get_values))
  stopifnot(is.logical(default))

  if (!all(map("logi", is.function, get_values))) {
    get_values <- Error("Value must be a list of functions.")
    throw(check_assignment(get_values))
  }

  as_tibble(list(param = param, get_values = get_values, default = default))
}


random_grid <- function(x, size = integer()) {
  stopifnot(is.list(x))

  names(x) <- make_names_along(x, "value")
  grids <- map(function(value, name) {
    do.call(tibble, structure(list(value), names = name))
  }, x, names(x))

  max_size <- prod(map("int", nrow, grids))
  if (is_empty(size)) size <- max_size
  size <- min(size, max_size)

  if (is_empty(grids) || size < 1) {
    grid <- as_tibble(map(function(data) data[NULL, 1, drop = TRUE], grids))
  } else {
    grid <- NULL
    iter <- 0
    while (size(grid, 1) < size && iter < 100) {
      iter <- iter + 1
      grid_sample <- as_tibble(map(function(data) {
        inds <- sample.int(nrow(data), size = size, replace = TRUE)
        data[inds, 1, drop = TRUE]
      }, grids))
      grid <- unique_grid(rbind(grid, grid_sample))
    }

    grid <- head(grid, size)
    cols <- unname(unnest_params(grid))
    sortable_types <- c("character", "complex", "Date", "factor", "logical",
                        "numeric")
    is_sortable <- map("logi", function(col) {
      any(map("logi", is, list(col), sortable_types))
    }, cols)
    if (any(is_sortable)) {
      sort_order <- do.call(order, cols[is_sortable])
      grid <- grid[sort_order, ]
    }
  }

  grid
}


regular_grid <- function(x) {
  stopifnot(is.list(x))

  names(x) <- make_names_along(x, "value")
  grids <- map(function(value, name) {
    do.call(tibble, structure(list(value), names = name))
  }, x, names(x))

  ns <- map("int", nrow, grids)
  n <- prod(ns)

  if (is_empty(grids) || n == 0) {
    repeats <- 0
  } else {
    ns_cumprod <- cumprod(ns)
    grids <- map(function(data, each) {
      inds <- rep(seq_len(nrow(data)), each = each)
      data[inds, ]
    }, grids, n / ns_cumprod)
    repeats <- ns_cumprod / ns
  }
  grids <- map(function(data, times) {
    inds <- rep(seq_len(nrow(data)), times = times)
    data[inds, 1, drop = TRUE]
  }, grids, repeats)

  as_tibble(grids)
}


renest_params <-function(x, template, drop = FALSE) {
  res <- template
  ind <- 1
  for (name in names(template)) {
    node <- template[[name]]
    res[[name]] <- if (is_tibble(node)) {
      size <- length(unnest_params(node))
      renest_params(x[seq(ind, length = size)], node, drop = TRUE)
    } else {
      size <- 1
      x[[ind]]
    }
    ind <- ind + size
  }
  if (length(res) || !drop) res
}


unique_grid <- function(x) {
  if (is_empty(x)) x else x[!duplicated(unnest_params(x)), ]
}


unnest_params <- function(x, compact_names = FALSE) {
  res <- tibble(.rows = nrow(x))
  for (name in names(x)) {
    value <- x[[name]]
    if (is_tibble(value)) {
      value <- unnest_params(value)
      if (compact_names) name <- c(name, character(length(value) - 1))
      name <- paste0(name, "$", names(value))
    }
    res[name] <- value
  }
  res
}
