#' Selected Model
#'
#' Model selection from a candidate set.
#'
#' @param ... \link[=models]{model} functions, function names, objects; other
#'   objects that can be \link[=as.MLModel]{coerced} to models; or vectors of
#'   these to serve as the candidate set from which to select, such as that
#'   returned by \code{\link{expand_model}}.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Model
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for model selection.
#' @param cutoff argument passed to the \code{metrics} functions.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#'
#' @return \code{SelectedModel} class object that inherits from \code{MLModel}.
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
SelectedModel <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff")
) {

  models <- as.list(unlist(list(...)))
  model_names <- character()
  for (i in seq_along(models)) {
    models[[i]] <- as.MLModel(models[[i]])
    name <- names(models)[i]
    model_names[i] <-
      if (!is.null(name) && nzchar(name)) name else models[[i]]@name
  }
  names(models) <- make.unique(model_names)

  slots <- combine_model_slots(models, settings("response_types"))
  new("SelectedModel", MLModel(
    name = "SelectedModel",
    label = "Selected Model",
    response_types = slots$response_types,
    weights = slots$weights,
    params = list(
      models = ListOf(models), control = as.MLControl(control),
      metrics = metrics, stat = stat, cutoff = cutoff
    )
  ))

}

MLModelFunction(SelectedModel) <- NULL


.fit.SelectedModel <- function(object, input, ...) {
  models <- object@params$models
  train_step <- resample_selection(models, identity, object@params, input,
                                   name = "SelectedModel")
  train_step@grid$params <- factor(seq_along(models))
  selected <- which(train_step@grid$selected)
  model <- models[[selected]]
  push(train_step, fit(input, model = model))
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
#'   \code{\link{Grid}} function, function name, or object;
#'   \code{\link{ParameterGrid}} object; or \link[=data.frame]{data frame}
#'   containing parameter values at which to evaluate the model, such as that
#'   returned by \code{\link{expand_params}}.
#' @param fixed list or one-row data frame with columns of fixed parameter
#'   values to combine with those in \code{grid}.
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
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#'
#' @return \code{TunedModel} class object that inherits from \code{MLModel}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
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
#'     model = TunedModel(GBMModel, grid = Grid(size = 1000, random = 5)))
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
  object, grid = MachineShop::settings("grid"), fixed = list(),
  control = MachineShop::settings("control"), metrics = NULL,
  stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff")
) {

  if (missing(object)) {
    object <- NULL
    response_types <- settings("response_types")
    weights <- FALSE
  } else {
    object <- if (is(object, "MLModel")) fget(object@name) else fget(object)
    stopifnot(is(object, "MLModelFunction"))
    model <- as.MLModel(object)
    response_types <- model@response_types
    weights <- model@weights
  }

  grid <- check_grid(grid)
  if (is(grid, "DomainError")) {
    value <- grid$value
    if (is(value, "data.frame")) {
      grid <- as_tibble(value)
    } else {
      grid$message <- paste0(grid$message,
                             "; a ParameterGrid object; or a data frame")
      throw(check_assignment(grid))
    }
  }

  fixed <- as_tibble(fixed)
  if (nrow(fixed) > 1) {
    throw(Error("only single values allowed for fixed parameters"))
  }

  new("TunedModel", MLModel(
    name = "TunedModel",
    label = "Grid Tuned Model",
    response_types = response_types,
    weights = weights,
    params = list(
      model = object, grid = grid, fixed = fixed,
      control = as.MLControl(control), metrics = metrics, stat = stat,
      cutoff = cutoff
    )
  ))

}

MLModelFunction(TunedModel) <- NULL


.fit.TunedModel <- function(object, input, ...) {
  params <- object@params
  grid <- expand_modelgrid(object, input)
  models <- expand_model(list(params$model, grid))
  train_step <- resample_selection(models, identity, params, input,
                                   name = "TunedModel")
  train_step@grid$params <- grid
  selected <- which(train_step@grid$selected)
  model <- models[[selected]]
  push(train_step, fit(input, model = model))
}
