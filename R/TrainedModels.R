#' Selected Model
#'
#' Model selection from a candidate set.
#'
#' @rdname SelectedModel
#'
#' @param ... \link[=models]{model} functions, function names, objects; other
#'   objects that can be \link[=as.MLModel]{coerced} to models; vectors of
#'   these to serve as the candidate set from which to select, such as that
#'   returned by \code{\link{expand_model}}; or model
#'   \link[=ModelSpecification]{specifications}.
#' @param x list of models followed by arguments passed to their method
#'   function.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Model
#'   selection is based on the first calculated metric.
#' @param cutoff argument passed to the \code{metrics} functions.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for model selection.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#'
#' @return \code{SelectedModel} or \code{SelectedModelSpecification} class
#' object that inherits from \code{MLModel} or \code{ModelSpecification},
#' respectively.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm and glmnet to run
#'
#' model_fit <- fit(
#'   sale_amount ~ ., data = ICHomes,
#'   model = SelectedModel(GBMModel, GLMNetModel, SVMRadialModel)
#' )
#' (selected_model <- as.MLModel(model_fit))
#' summary(selected_model)
#' }
#'
SelectedModel <- function(...) {
  UseMethod("SelectedModel")
}

MLModelFunction(SelectedModel) <- NULL


#' @rdname SelectedModel
#'
SelectedModel.default <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {

  models <- as.list(unlist(list(...)))
  for (i in seq_along(models)) models[[i]] <- as.MLModel(models[[i]])

  if (length(models) == 1) return(models[[1]])

  default_names <- map("char", slot, models, "name")
  names(models) <- make_names_along(models, default_names)

  slots <- combine_model_slots(models, settings("response_types"))
  new("SelectedModel",
    MLModel(
      name = "SelectedModel",
      label = "Selected Model",
      response_types = slots$response_types,
      weights = slots$weights,
      params = TrainingParams(
        control = control,
        metrics = metrics,
        cutoff = cutoff,
        stat = stat
      )
    ),
    candidates = ListOf(models)
  )

}


#' @rdname SelectedModel
#'
SelectedModel.ModelSpecification <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {
  do.call(SelectedInput, c(list(...), as.list(environment())))
}


#' @rdname SelectedModel
#'
SelectedModel.list <- function(x, ...) {
  do.call(SelectedModel, c(x, list(...)))
}


.fit.SelectedModel <- function(object, ...) {
  .fit_optim(object, ...)
}


update.SelectedModel <- function(object, params = NULL, ...) {
  object <- subset_selected(object, "candidates", params$id)
  params$id <- NULL
  NextMethod()
}


#' Tuned Model
#'
#' Model tuning over a grid of parameter values.
#'
#' @param object \link[=models]{model} function, function name, or object
#'   defining the model to be tuned.
#' @param grid single integer or vector of integers whose positions or names
#'   match the parameters in the model's pre-defined tuning grid if one exists
#'   and which specify the number of values used to construct the grid;
#'   \code{\link{TuningGrid}} function, function name, or object;
#'   \code{\link{ParameterGrid}} object; or \link[=data.frame]{data frame}
#'   containing parameter values at which to evaluate the model, such as that
#'   returned by \code{\link{expand_params}}.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Model
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for model tuning.
#' @param cutoff argument passed to the \code{metrics} functions.
#'
#' @details
#' The \code{\link{expand_modelgrid}} function enables manual extraction and
#' viewing of grids created automatically when a \code{TunedModel} is fit.
#'
#' \describe{
#'   \item{Response types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#'
#' @return \code{TunedModel} class object that inherits from \code{MLModel}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{set_optim}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#' ## May require a long runtime
#'
#' # Automatically generated grid
#' model_fit <- fit(sale_amount ~ ., data = ICHomes,
#'                  model = TunedModel(GBMModel))
#' varimp(model_fit)
#' (tuned_model <- as.MLModel(model_fit))
#' summary(tuned_model)
#' plot(tuned_model, type = "l")
#'
#' # Randomly sampled grid points
#' fit(sale_amount ~ ., data = ICHomes,
#'     model = TunedModel(
#'       GBMModel,
#'       grid = TuningGrid(size = 1000, random = 5)
#'     ))
#'
#' # User-specified grid
#' fit(sale_amount ~ ., data = ICHomes,
#'     model = TunedModel(
#'       GBMModel,
#'       grid = expand_params(
#'         n.trees = c(50, 100),
#'         interaction.depth = 1:2,
#'         n.minobsinnode = c(5, 10)
#'       )
#'     ))
#' }
#'
TunedModel <- function(
  object, grid = MachineShop::settings("grid"),
  control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {

  if (missing(object)) {
    object <- NullModel()
    response_types <- settings("response_types")
    weights <- FALSE
  } else {
    object <- as.MLModel(object)
    response_types <- object@response_types
    weights <- object@weights
  }

  params <- names(formals(fget(object@name)))
  params_type <- "function"
  grid <- check_grid(grid)
  if (is(grid, "DomainError")) {
    value <- grid$value
    if (is(value, "data.frame")) {
      grid <- as_tibble(value)
      grid_params <- names(grid)
    } else {
      grid$message <- paste0(
        grid$message, "; a ParameterGrid object; or a data frame"
      )
      throw(check_assignment(grid))
    }
  } else if (is(grid, "ParameterGrid")) {
    grid_params <- grid$id
  } else {
    params <- object@gridinfo$param
    params_type <- "grid"
    grid_params <- names(grid@size)
    if (is.null(grid_params)) {
      if (length(grid@size) == 1) {
        if (!grid@random) params <- params[object@gridinfo$default]
        grid_params <- params
        grid@size <- rep(grid@size, max(length(grid_params), 1))
        names(grid@size) <- grid_params
      } else {
        grid_params <- params[seq_along(grid@size)]
        names(grid@size) <- grid_params
      }
    }
  }
  if (!("..." %in% params || all(grid_params %in% params))) {
    begin <- function(subject) paste0("x ", subject, " has parameter{?s}: ")
    throw(Error(
      "Unmatched tuning parameters.\n",
      note_items(begin(paste(object@name, params_type)), params, ".\n"),
      note_items(begin("Argument `grid`"), grid_params, ".")
    ))
  }

  new("TunedModel",
    MLModel(
      name = "TunedModel",
      label = paste("Grid Tuned", object@name),
      response_types = response_types,
      weights = weights,
      params = TrainingParams(
        control = control,
        metrics = metrics,
        cutoff = cutoff,
        stat = stat
      )
    ),
    model = object,
    grid = grid
  )

}

MLModelFunction(TunedModel) <- NULL


.fit.TunedModel <- function(object, ...) {
  .fit_optim(object, ...)
}


update.TunedModel <- function(
  object, params = NULL, ...
) {
  if (is.list(params)) {
    update(object@model, params = params, id = object@id)
  } else {
    object
  }
}
