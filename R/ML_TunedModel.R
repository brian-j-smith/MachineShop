#' Tuned Model
#' 
#' Model tuning over a grid of parameter values.
#' 
#' @param model \link[=models]{model} function, function name, or call defining
#'   the model to be tuned.
#' @param grid \link[=data.frame]{data frame} containing parameter values at
#'   which to evaluate a single model supplied to \code{models}, such as that
#'   returned by \code{\link{expand_params}}; the number of parameter-specific
#'   values to generate automatically if the model has a pre-defined grid; or a
#'   call to \code{\link{Grid}}.
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
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#' 
#' @return \code{TunedModel} class object that inherits from \code{MLModel}.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = TunedModel(GLMNetModel))
#' 
TunedModel <- function(model, grid = MachineShop::settings("grid"),
                       fixed = NULL, control = MachineShop::settings("control"),
                       metrics = NULL,
                       stat = MachineShop::settings("stat.Tune"),
                       cutoff = MachineShop::settings("cutoff")) {
  
  if (missing(model)) {
    model <- NULL
  } else {
    model <- if (is(model, "MLModel")) fget(model@name) else fget(model)
    stopifnot(is(model, "MLModelFunction"))
  }
  
  if (is(grid, "numeric")) {
    grid <- Grid(grid)
  } else if (identical(grid, "Grid") || identical(grid, Grid)) {
    grid <- Grid()
  } else if (is(grid, "data.frame")) {
    grid <- as_tibble(grid)
  } else if (!is(grid, "Grid")) {
    stop("'grid' must be a grid length, Grid object, or data frame")
  }
  
  fixed <- as_tibble(fixed)
  if (nrow(fixed) > 1) stop("only single values allowed for fixed parameters")
  
  new("TunedModel",
    name = "TunedModel",
    label = "Grid Tuned Model",
    response_types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    predictor_encoding = NA_character_,
    params = list(model = model, grid = grid, fixed = fixed,
                  control = getMLObject(control, "MLControl"),
                  metrics = metrics, stat = stat, cutoff = cutoff)
  )
  
}

MLModelFunction(TunedModel) <- NULL


.fit.TunedModel <- function(model, x, ...) {
  fit(x, model = tune(model, x))
}
