#' Selected Model
#'
#' Model selection from a candidate set.
#'
#' @param ... \link[=models]{model} functions, function names, calls, or vectors
#'   of these to serve as the candidate set from which to select, such as that
#'   returned by \code{\link{expand_model}}.
#' @param control \link[=controls]{control} function, function name, or call
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
#' model_fit <- fit(sale_amount ~ ., data = ICHomes,
#'                  model = SelectedModel(GBMModel, GLMNetModel, SVMRadialModel))
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
  for (i in seq(models)) {
    models[[i]] <- get_MLObject(models[[i]], class = "MLModel")
    name <- names(models)[i]
    model_names[i] <-
      if (!is.null(name) && nzchar(name)) name else models[[i]]@name
  }
  names(models) <- make.unique(model_names)

  new("SelectedModel",
      name = "SelectedModel",
      label = "Selected Model",
      response_types = Reduce(intersect,
                              map(slot, models, "response_types"),
                              init = settings("response_types")),
      predictor_encoding = NA_character_,
      params = list(models = ListOf(models),
                    control = get_MLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff)
  )

}

MLModelFunction(SelectedModel) <- NULL


.fit.SelectedModel <- function(x, inputs, ...) {
  models <- x@params$models
  train_step <- resample_selection(models, identity, x@params, inputs,
                                   class = "SelectedModel")
  train_step$grid <- tibble(Model = factor(seq(models)))
  model <- models[[train_step$selected]]
  push(do.call(TrainStep, train_step), fit(inputs, model = model))
}


#' Tuned Model
#'
#' Model tuning over a grid of parameter values.
#'
#' @param model \link[=models]{model} function, function name, or call defining
#'   the model to be tuned.
#' @param grid single integer or vector of integers whose positions or names
#'   match the parameters in the model's pre-defined tuning grid if one exists
#'   and which specify the number of values used to construct the grid;
#'   \code{\link{Grid}} function, function call, or object;
#'   \code{\link{ParameterGrid}} object; or \link[=data.frame]{data frame}
#'   containing parameter values at which to evaluate the model, such as that
#'   returned by \code{\link{expand_params}}.
#' @param fixed list of fixed parameter values to combine with those in
#'   \code{grid}.
#' @param control \link[=controls]{control} function, function name, or call
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
#'     model = TunedModel(GBMModel,
#'                        grid = expand_params(n.trees = c(50, 100),
#'                                             interaction.depth = 1:2,
#'                                             n.minobsinnode = c(5, 10))))
#' }
#'
TunedModel <- function(
  model, grid = MachineShop::settings("grid"), fixed = list(),
  control = MachineShop::settings("control"), metrics = NULL,
  stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff")
) {

  if (missing(model)) {
    model <- NULL
  } else {
    model <- if (is(model, "MLModel")) fget(model@name) else fget(model)
    stopifnot(is(model, "MLModelFunction"))
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

  new("TunedModel",
      name = "TunedModel",
      label = "Grid Tuned Model",
      response_types = if (is.null(model)) {
        settings("response_types")
      } else {
        model()@response_types
      },
      predictor_encoding = NA_character_,
      params = list(model = model, grid = grid, fixed = fixed,
                    control = get_MLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff)
  )

}

MLModelFunction(TunedModel) <- NULL


.fit.TunedModel <- function(x, inputs, ...) {
  params <- x@params
  grid <- expand_modelgrid(x, inputs)
  models <- expand_model(list(params$model, grid))
  train_step <- resample_selection(models, identity, params, inputs,
                                   class = "TunedModel")
  train_step$grid <- tibble(Model = grid)
  model <- models[[train_step$selected]]
  push(do.call(TrainStep, train_step), fit(inputs, model = model))
}
