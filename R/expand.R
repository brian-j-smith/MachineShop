#' Model Expansion Over Tuning Parameters
#'
#' Expand a model over all combinations of a grid of tuning parameters.
#'
#' @param object \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.
#' @param ... named vectors or factors or a list of these containing the
#'   parameter values over which to expand \code{object}.
#' @param random number of points to be randomly sampled from the parameter grid
#'   or \code{FALSE} if all points are to be returned.
#'
#' @return \code{list} of expanded models.
#'
#' @seealso \code{\link{SelectedModel}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' data(Boston, package = "MASS")
#'
#' models <- expand_model(GBMModel, n.trees = c(50, 100),
#'                                  interaction.depth = 1:2)
#'
#' fit(medv ~ ., data = Boston, model = SelectedModel(models))
#' }
#'
expand_model <- function(object, ..., random = FALSE) {
  .expand_model(object, ..., random = random)
}


.expand_model <- function(object, ...) {
  UseMethod(".expand_model")
}


.expand_model.default <- function(object, ..., random) {
  expand_model(as.MLModel(object), ..., random = random)
}


.expand_model.tbl_df <- function(object, model, ...) {
  models <- map(function(params) {
    do.call(update, list(model, params = params, new_id = TRUE), quote = TRUE)
  }, split(object, seq_len(max(nrow(object), 1))))
  names(models) <- paste0(models[[1]]@name, ".", names(models))
  models
}


.expand_model.MLModel <- function(object, ..., random) {
  expand_model(expand_params(..., random = random), object)
}


#' Model Tuning Grid Expansion
#'
#' Expand a model grid of tuning parameter values.
#'
#' @name expand_modelgrid
#' @rdname expand_modelgrid-methods
#'
#' @param ... arguments passed from the generic function to its methods and from
#'   the \code{MLModel} and \code{MLModelFunction} methods to others.  The
#'   first argument of each \code{expand_modelgrid} method is positional and, as
#'   such, must be given first in calls to them.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param input \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param object model \link[=ModelSpecification]{specification}.
#' @param model \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.  A model
#'   can be given first followed by any of the variable specifications.
#' @param info logical indicating whether to return model-defined grid
#'   construction information rather than the grid values.
#'
#' @details
#' The \code{expand_modelgrid} function enables manual extraction and viewing of
#' grids created automatically when a \code{\link{TunedModel}} is fit.
#'
#' @return A data frame of parameter values or \code{NULL} if data are required
#' for construction of the grid but not supplied.
#'
#' @seealso \code{\link{TunedModel}}
#'
#' @examples
#' expand_modelgrid(TunedModel(GBMModel, grid = 5))
#'
#' expand_modelgrid(TunedModel(GLMNetModel, grid = c(alpha = 5, lambda = 10)),
#'                  sale_amount ~ ., data = ICHomes)
#'
#' gbm_grid <- ParameterGrid(
#'   n.trees = dials::trees(),
#'   interaction.depth = dials::tree_depth(),
#'   size = 5
#' )
#' expand_modelgrid(TunedModel(GBMModel, grid = gbm_grid))
#'
#' rf_grid <- ParameterGrid(
#'   mtry = dials::mtry(),
#'   nodesize = dials::max_nodes(),
#'   size = c(3, 5)
#' )
#' expand_modelgrid(TunedModel(RandomForestModel, grid = rf_grid),
#'                  sale_amount ~ ., data = ICHomes)
#'
expand_modelgrid <- function(...) {
  UseMethod("expand_modelgrid")
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.formula <- function(formula, data, model, info = FALSE, ...) {
  expand_modelgrid(model, formula, data, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.matrix <- function(x, y, model, info = FALSE, ...) {
  .expand_modelgrid(as.MLModel(model), x, y, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.ModelFrame <- function(input, model, info = FALSE, ...) {
  .expand_modelgrid(as.MLModel(model), input, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.recipe <- function(input, model, info = FALSE, ...) {
  .expand_modelgrid(as.MLModel(model), input, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.ModelSpecification <- function(object, ...) {
  grid <- object@grid

  if (is_empty(grid)) {

    types <- c("ModeledInput", "SelectedInput", "SelectedModel")
    found <- unique(unlist(map_slots(function(slot) {
      types[map("logi", is, list(slot), types)]
    }, object)))
    if (length(found)) {
      throw(Warning(
        "Cannot expand the tuning grids of a ModelSpecification containing ",
        as_string(found, conj = "or"), "."
      ))
      return(NULL)
    }

    get_modelgrids <- function(input) {
      map_slots(function(model) {
        get_grid(model, input)$params
      }, object, names = c("model", "candidates"))
    }

    make_grid <- function(grids) {
      expand_params(grids[!map("logi", is_empty, grids)])
    }

    input_grid <- make_grid(map_slots(function(input) {
      get_grid(input)$params
    }, object, names = c("input", "candidates")))

    if (nrow(input_grid)) {
      grid <- NULL
      for (i in seq_len(nrow(input_grid))) {
        params <- input_grid[i, ]
        input <- update_slots(object@input, params = params)
        grid <- rbind(grid, make_grid(c(params, get_modelgrids(input))))
      }
    } else {
      grid <- make_grid(get_modelgrids(object@input))
    }

  }

  grid
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.MLModel <- function(model, ...) {
  .expand_modelgrid(model, ...)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.MLModelFunction <- function(model, ...) {
  expand_modelgrid(as.MLModel(model), ...)
}


.expand_modelgrid <- function(object, ...) {
  UseMethod(".expand_modelgrid")
}


.expand_modelgrid.MLModel <- function(object, ..., info = FALSE) {
  if (info) object@gridinfo else tibble()
}


.expand_modelgrid.TunedModel <- function(object, ..., info = FALSE) {
  if (info) {
    NextMethod()
  } else {
    random <- if (is_optim_method(object, "RandomGridSearch")) {
      get_optim_field(object, "size")
    } else FALSE
    .expand_modelgrid(object@grid, ..., model = object@model, random = random)
  }
}


.expand_modelgrid.TuningGrid <- function(object, input, ..., model, random) {
  gridinfo <- model@gridinfo
  size <- object@size
  if (object@random) random <- object@random
  mf <- NULL

  not_dup <- function(x) !duplicated(x, fromLast = TRUE)
  if (!is.null(names(size))) {
    size <- size[gridinfo$param] * not_dup(gridinfo$param)
    size[is.na(size)] <- 0L
  } else if (length(size) == 1) {
    if (!random) gridinfo <- gridinfo[gridinfo$default, ]
    size <- size * not_dup(gridinfo$param)
  }
  gridinfo$size <- size
  gridinfo <- gridinfo[gridinfo$size >= 1, ]

  has_data_arg <- function(fun) "data" %in% names(formals(fun))
  needs_data <- any(map("logi", has_data_arg, gridinfo$get_values))
  if (needs_data && has_grid(model)) {
    if (!missing(input)) mf <- ModelFrame(input, ..., na.rm = FALSE)
    if (is.null(mf)) {
      return(NULL)
    }
    y <- response(mf)
    if (!any(is_response(y, model@response_types))) {
      throw(LocalWarning(
        "Invalid model response type in expand_modelgrid().\n",
        "x Exising ", model@name, " supports ",
        note_items("type{?s}: ", model@response_types), ".\n",
        "x Supplied response is of ", note_items("type{?s}: ", class(y)), "."
      ))
      return(NULL)
    }
  }

  param_names <- unique(gridinfo$param)
  params <- map(function(fun, n) unique(fun(n = n, data = mf)),
                gridinfo$get_values, gridinfo$size)
  params <- map(function(name) {
    unlist(params[gridinfo$param == name], use.names = FALSE)
  }, param_names)
  names(params) <- param_names
  params[lengths(params) == 0] <- NULL
  grid <- expand_params(params, random = random)

  .expand_modelgrid(grid)
}


.expand_modelgrid.ParameterGrid <- function(object, input, ..., model, random) {
  grid <- if (nrow(object)) {
    needs_data <- any(dials::has_unknowns(object$object))
    if (needs_data) {
      if (missing(input)) return(NULL)
      mf <- ModelFrame(input, ..., na.rm = FALSE)
      data <- switch(model@predictor_encoding,
        "model.frame" = {
          mf_terms <- attributes(terms(mf))
          var_list <- eval(mf_terms$variables, mf)
          names(var_list) <- rownames(mf_terms$factors)
          as.data.frame(var_list[-c(1, mf_terms$offset)])
        },
        "model.matrix" = model.matrix(mf, intercept = FALSE)
      )
      object <- dials::finalize(object, x = data)
    }
    params <- map(dials::value_seq, object$object, object@size)
    expand_params(params, random = if (object@random) object@random else random)
  } else {
    tibble()
  }

  .expand_modelgrid(grid)
}


.expand_modelgrid.tbl_df <- function(object, ...) {
  if (ncol(object)) object[!duplicated(object), ] else object
}


#' Model Parameters Expansion
#'
#' Create a grid of parameter values from all combinations of supplied inputs.
#'
#' @param ... named data frames or vectors or a list of these containing the
#'   parameter values over which to create the grid.
#' @param random number of points to be randomly sampled from the parameter grid
#'   or \code{FALSE} if all points are to be returned.
#'
#' @return A data frame containing one row for each combination of the supplied
#' inputs.
#'
#' @seealso \code{\link{TunedModel}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' data(Boston, package = "MASS")
#'
#' grid <- expand_params(
#'   n.trees = c(50, 100),
#'   interaction.depth = 1:2
#' )
#'
#' fit(medv ~ ., data = Boston, model = TunedModel(GBMModel, grid = grid))
#' }
#'
expand_params <- function(..., random = FALSE) {
  x <- list(...)
  if (is_one_element(x, "list") && is.null(names(x))) x <- x[[1]]
  if (random) {
    random_grid(x, size = random)
  } else {
    regular_grid(x)
  }
}


#' Recipe Step Parameters Expansion
#'
#' Create a grid of parameter values from all combinations of lists supplied for
#' steps of a preprocessing recipe.
#'
#' @param ... one or more lists containing parameter values over which to create
#'   the grid.  For each list an argument name should be given as the \code{id}
#'   of the \link[recipes]{recipe} step to which it corresponds.
#' @param random number of points to be randomly sampled from the parameter grid
#'   or \code{FALSE} if all points are to be returned.
#'
#' @return \code{RecipeGrid} class object that inherits from \code{data.frame}.
#'
#' @seealso \code{\link{TunedInput}}
#'
#' @examples
#' library(recipes)
#' data(Boston, package = "MASS")
#'
#' rec <- recipe(medv ~ ., data = Boston) %>%
#'   step_corr(all_numeric_predictors(), id = "corr") %>%
#'   step_pca(all_numeric_predictors(), id = "pca")
#'
#' expand_steps(
#'   corr = list(threshold = c(0.8, 0.9),
#'               method = c("pearson", "spearman")),
#'   pca = list(num_comp = 1:3)
#' )
#'
expand_steps <- function(..., random = FALSE) {

  steps <- list(...)
  step_names <- names(steps)
  if (is_one_element(steps, "list") && is.null(step_names)) {
    steps <- steps[[1]]
    step_names <- names(steps)
  }

  if (!all(map("logi", is.list, steps))) {
    throw(Error("Step arguments must be lists."))
  }

  get_names <- function(x) {
    res <- NULL
    if (is.list(x)) {
      for (i in seq_along(x)) {
        name <- names(x[i])
        if (is.null(name)) name <- ""
        res <- c(res, name, get_names(x[[i]]))
      }
    }
    res
  }

  if (!all(nzchar(get_names(steps)))) {
    throw(Error("All steps and their parameters must be named."))
  } else if (any(duplicated(step_names))) {
    throw(Error("Step names must be unique."))
  }

  steps <- map(expand_params, steps, random = random)
  RecipeGrid(expand_params(steps, random = random))

}
