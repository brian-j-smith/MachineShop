#' Model Expansion Over Tuning Parameters
#'
#' Expand a model over all combinations of a grid of tuning parameters.
#'
#' @param x \link[=models]{model} function, function name, or call.
#' @param ... named vectors or factors or a list of these containing the
#'   parameter values over which to expand \code{x}.
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
expand_model <- function(x, ..., random = FALSE) {
  .expand_model(x, random, ...)
}


.expand_model <- function(x, ...) {
  UseMethod(".expand_model")
}


.expand_model.default <- function(x, random, ...) {
  expand_model(get_MLModel(x), ..., random = random)
}


.expand_model.list <- function(x, ...) {
  grid <- x[[2]]
  models <- map(function(args) do.call(x[[1]], args),
                split(grid, seq_len(max(1, nrow(grid)))))
  names(models) <- paste0(models[[1]]@name, ".", names(models))
  models
}


.expand_model.MLModel <- function(x, random, ...) {
  grid <- expand_params(..., random = random)
  expand_model(list(fget(x@name), grid))
}


#' Model Tuning Grid Expansion
#'
#' Expand a model grid of tuning parameter values.
#'
#' @name expand_modelgrid
#' @rdname expand_modelgrid-methods
#'
#' @param x \link[=inputs]{input} specifying a relationship between model
#'   predictor and response variables.  Alternatively, a
#'   \code{\link{TunedModel}} object may be given first followed optionally by
#'   an input specification.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \code{\link{TunedModel}} object.
#' @param info logical indicating whether to return model-defined grid
#'   construction information rather than the grid values.
#' @param ... arguments passed to other methods.
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
expand_modelgrid <- function(x, ...) {
  UseMethod("expand_modelgrid")
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.formula <- function(x, data, model, info = FALSE, ...) {
  expand_modelgrid(model, x, data, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.matrix <- function(x, y, model, info = FALSE, ...) {
  expand_modelgrid(model, x, y, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.ModelFrame <- function(x, model, info = FALSE, ...) {
  expand_modelgrid(model, x, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.recipe <- function(x, model, info = FALSE, ...) {
  expand_modelgrid(model, x, info = info)
}


#' @rdname expand_modelgrid-methods
#'
expand_modelgrid.TunedModel <- function(x, ..., info = FALSE) {
  params <- x@params
  model <- params$model()
  if (info) {
    model@gridinfo
  } else {
    .expand_modelgrid(params$grid, ..., model = model, fixed = params$fixed)
  }
}


.expand_modelgrid <- function(grid, ...) {
  UseMethod(".expand_modelgrid")
}


.expand_modelgrid.Grid <- function(grid, x, ..., model, fixed) {
  gridinfo <- model@gridinfo
  size <- grid@size
  random <- grid@random
  mf <- NULL

  not_dup <- function(x) !duplicated(x, fromLast = TRUE)
  if (!is.null(names(size))) {
    if (!all(names(size) %in% gridinfo$param)) {
      throw(LocalWarning(
        "Unmatched model parameters in expand_modelgrid() argument 'size'.\n",
        "x Existing ", model@name, " has ",
        label_items("parameter", gridinfo$param), ".\n",
        "x Assigned data has ", label_items("name", names(size)), "."
      ))
    }
    size <- size[gridinfo$param] * not_dup(gridinfo$param)
    size[is.na(size)] <- 0L
  } else if (length(size) == 1) {
    if (!random) gridinfo <- gridinfo[gridinfo$default, ]
    size <- size * not_dup(gridinfo$param)
  } else if (length(size) != nrow(gridinfo)) {
    throw(LocalError(
      "Length of expand_modelgrid() argument 'size' must equal 1 ",
      "or the number of model parameters.\n",
      "x Existing ", model@name, " has ", nrow(gridinfo), " ",
      label_items("parameter", gridinfo$param), ".\n",
      "x Assigned data has ", length(size), " ", label_items("size", size), "."
    ))
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
      throw(LocalWarning(
        "Invalid model response type in expand_modelgrid().\n",
        "x Exising ", model@name, " supports ",
        label_items("type", model@response_types), ".\n",
        "x Supplied response is of ", label_items("type", class(y)), "."
      ))
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
  grid <- expand_params(params, random = random)

  .expand_modelgrid(grid, fixed = fixed)
}


.expand_modelgrid.ParameterGrid <- function(grid, x, ..., model, fixed) {
  grid <- if (nrow(grid)) {
    needs_data <- any(dials::has_unknowns(grid$object))
    if (needs_data) {
      if (missing(x)) return(NULL)
      mf <- ModelFrame(x, ..., na.rm = FALSE)
      model <- get_MLModel(model)
      data <- switch(model@predictor_encoding,
                     "model.matrix" = model.matrix(mf, intercept = FALSE),
                     "terms" = {
                       mf_terms <- attributes(terms(mf))
                       var_list <- eval(mf_terms$variables, mf)
                       names(var_list) <- rownames(mf_terms$factors)
                       as.data.frame(var_list[-c(1, mf_terms$offset)])
                     }
      )
      grid <- dials::finalize(grid, x = data)
    }
    params <- map(dials::value_seq, grid$object, grid@size)
    expand_params(params, random = grid@random)
  } else {
    tibble()
  }

  .expand_modelgrid(grid, fixed = fixed)
}


.expand_modelgrid.tbl_df <- function(grid, fixed, ...) {
  if (!nrow(grid)) grid <- tibble(.rows = 1)
  grid[names(fixed)] <- fixed
  if (ncol(grid)) grid[!duplicated(grid), ] else grid
}


#' Model Parameters Expansion
#'
#' Create a grid of parameter values from all combinations of supplied inputs.
#'
#' @param ... named vectors or factors or a list of these containing the
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
  if (random) {
    x <- list(...)
    if (is_one_element(x, "list") && is.null(names(x))) x <- x[[1]]
    sample_params(x, size = random)
  } else {
    as_tibble(expand.grid(..., KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE))
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
#'   step_corr(all_numeric(), -all_outcomes(), id = "corr") %>%
#'   step_pca(all_numeric(), -all_outcomes(), id = "pca")
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

  if (!all(map_logi(is.list, steps))) {
    throw(Error("step arguments must be lists"))
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
    throw(Error("all steps and their parameters must be named"))
  } else if (any(duplicated(step_names))) {
    throw(Error("step names must be unique"))
  }

  grid <- expand_params(unlist(steps, recursive = FALSE), random = random)
  recipe_grid <- tibble(.rows = nrow(grid))

  offset <- 0
  for (name in step_names) {
    indices <- offset + seq_len(length(steps[[name]]))
    x <- grid[indices]
    names(x) <- substring(names(x), nchar(name) + 2)
    recipe_grid[[name]] <- x
    offset <- offset + length(indices)
  }

  RecipeGrid(recipe_grid)

}
