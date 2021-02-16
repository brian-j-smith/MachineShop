#' Model Tuning Grid
#'
#' Extract a model-defined grid of tuning parameter values.
#'
#' @name get_grid
#' @rdname get_grid-methods
#'
#' @param x optional \link[=inputs]{input} specifying a relationship between
#'   model predictor and response variables.  Alternatively, a
#'   \link[=models]{model} function or call may be given first followed by the
#'   input specification.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or call.
#' @param size single integer or vector of integers whose positions or names
#'   match the parameters in the model's tuning grid and which specify the
#'   number of values to use in constructing the grid.
#' @param random number of unique grid points to sample at random, \code{Inf}
#'   for all random points, or \code{FALSE} for all fixed points.
#' @param info logical indicating whether to return the grid construction
#'   information rather than the grid values.
#' @param ... arguments passed to the default method.
#'
#' @details The \code{get_grid} function enables manual extraction and viewing
#' of grids created automatically if \code{\link{TunedModel}} is called with a
#' \code{\link{Grid}} object.
#'
#' @return A data frame of parameter values or \code{NULL} if data are required
#' for construction of the grid but not supplied.
#'
#' @seealso \code{\link{TunedModel}}, \code{\link{Grid}}
#'
#' @examples
#' get_grid(GBMModel, size = 10)
#'
#' get_grid(sale_amount ~ ., data = ICHomes, model = GLMNetModel,
#'          size = c(lambda = 10, alpha = 5))
#'
get_grid <- function(x, ...) {
  UseMethod("get_grid", x)
}


#' @rdname get_grid-methods
#'
get_grid.default <- function(x, ..., model, size = 3, random = FALSE,
                             info = FALSE) {
  model <- getMLObject(model, "MLModel")
  gridinfo <- model@gridinfo
  mf <- NULL

  if (info) return(gridinfo)

  not_dup <- function(x) !duplicated(x, fromLast = TRUE)
  if (!is.null(names(size))) {
    if (!all(names(size) %in% gridinfo$param)) {
      warn("Unmatched model parameter names in get_grid() argument 'size'.\n",
           "x Existing ", model@name, " has ",
           label_items("parameter", gridinfo$param), ".\n",
           "x Assigned data has ", label_items("name", names(size)), ".")
    }
    size <- size[gridinfo$param] * not_dup(gridinfo$param)
    size[is.na(size)] <- 0L
  } else if (length(size) == 1) {
    if (!random) gridinfo <- gridinfo[gridinfo$regular, ]
    size <- size * not_dup(gridinfo$param)
  } else if (length(size) != nrow(gridinfo)) {
    stop("Length of get_grid() argument 'size' must equal 1",
         " or the number of model parameters.\n",
         "x Existing ", model@name, " has ", nrow(gridinfo), " ",
         label_items("parameter", gridinfo$param), ".\n",
         "x Assigned data has ", length(size), " ",
         label_items("size", size), ".",
         call. = FALSE)
  }
  gridinfo$size <- size
  gridinfo <- gridinfo[gridinfo$size >= 1, ]

  has_data_arg <- function(fun) "data" %in% names(formals(fun))
  needs_data <- any(map_logi(has_data_arg, gridinfo$values))
  if (needs_data && has_grid(model)) {
    if (!missing(x)) mf <- ModelFrame(x, ..., na.rm = FALSE)
    if (is.null(mf)) {
      return(NULL)
    } else if (!is_valid_response(y <- response(mf), model)) {
      warn("Invalid model response type in get_grid().\n",
           "x Exising ", model@name, " supports ",
           label_items("type", model@response_types), ".\n",
           "x Supplied response is of ",
           label_items("type", class(y)), ".")
      return(NULL)
    }
  }

  param_names <- unique(gridinfo$param)
  params <- map(function(fun, n) unique(fun(n = n, data = mf)),
                gridinfo$values, gridinfo$size)
  params <- map(function(name) {
    unlist(params[gridinfo$param == name], use.names = FALSE)
  }, param_names)
  names(params) <- param_names
  params[lengths(params) == 0] <- NULL
  expand_params(params, random = random)
}


#' @rdname get_grid-methods
#'
get_grid.formula <- function(x, data, ...) {
  get_grid.default(x, data, ...)
}


#' @rdname get_grid-methods
#'
get_grid.matrix <- function(x, y, ...) {
  get_grid.default(x, y, ...)
}


#' @rdname get_grid-methods
#'
get_grid.ModelFrame <- function(x, ...) {
  get_grid.default(x, ...)
}


#' @rdname get_grid-methods
#'
get_grid.recipe <- function(x, ...) {
  get_grid.default(x, ...)
}


#' @rdname get_grid-methods
#'
get_grid.MLModel <- function(x, ...) {
  get_grid.default(..., model = x)
}


#' @rdname get_grid-methods
#'
get_grid.MLModelFunction <- function(x, ...) {
  get_grid(x(), ...)
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
#' @details Returned \code{Grid} objects may be supplied to
#' \code{\link{TunedModel}} for automated construction of model tuning grids.
#' These grids can be extracted manually and viewed with the
#' \code{\link{get_grid}} function.
#'
#' @return \code{Grid} class object.
#'
#' @seealso \code{\link{TunedModel}}, \code{\link{get_grid}}
#'
#' @examples
#' TunedModel(GBMModel, grid = Grid(10, random = 5))
#'
Grid <- function(size = 3, random = FALSE, length = NULL) {
  if (!is.null(length)) {
    depwarn("'length' argument to Grid is deprecated",
            "use 'size' argument instead", expired = Sys.Date() >= "2021-04-01")
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
ParameterGrid.list <- function(x, size = 3, random = FALSE, length = NULL, ...) {
  ParameterGrid(parameters(x), size = size, random = random, length = length)
}


#' @rdname ParameterGrid
#'
ParameterGrid.parameters <- function(x, size = 3, random = FALSE, length = NULL, ...) {
  if (!is.null(length)) {
    depwarn("'length' argument to ParameterGrid is deprecated",
            "use 'size' argument instead", expired = Sys.Date() >= "2021-04-01")
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
  grid <- get_grid(..., model = model, size = x@size, random = x@random)
  as.grid(grid, fixed = fixed)
}


as.grid.ParameterGrid <- function(x, ..., model, fixed = tibble()) {
  grid <- if (nrow(x)) {
    if (any(map_logi(dials::has_unknowns, x$object))) {
      mf <- ModelFrame(..., na.rm = FALSE)
      data <- switch(model@predictor_encoding,
        "model.matrix" = model.matrix(mf, intercept = FALSE),
        "terms" = {
          mf_terms <- attributes(terms(mf))
          var_list <- eval(mf_terms$variables, mf)
          names(var_list) <- rownames(mf_terms$factors)
          as.data.frame(var_list[-c(1, mf_terms$offset)])
        }
      )
      x <- dials::finalize(x, x = data)
    }
    if (x@random) {
      dials::grid_random(x, size = x@random)
    } else {
      dials::grid_regular(x, levels = x@size)
    }
  } else {
    tibble(.rows = 1)
  }
  as.grid(grid, fixed = fixed)
}
