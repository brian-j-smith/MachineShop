#' Model Specification
#'
#' Specification of a relationship between response and predictor variables and
#' a model to define a relationship between them.
#'
#' @name ModelSpecification
#' @rdname ModelSpecification-methods
#'
#' @param ... arguments passed from the generic function to its methods.  The
#'   first arguments of \code{ModelSpecification} methods are positional and, as
#'   such, must be given first in calls to them.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param input \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param model \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.  The
#'   argument can be omitted altogether in the case of
#'   \link[=ModeledInput]{modeled inputs}.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.  Specification of a
#'   resampling method results in the following characteristics.
#'   \enumerate{
#'     \item Tuning of input and model objects is performed simultaneously over
#'       a global grid of their parameter values.
#'     \item The specification's \code{control} method and training parameters
#'       below override those of any included \code{TunedInput} or
#'       \code{TunedModel}.
#'     \item \code{ModeledInput}, \code{SelectedInput}, and \code{SelectedModel}
#'       objects are not allowed in the \code{input} or \code{model} arguments.
#'   }
#'   Alternatively, a value of \code{NULL} may be specified so that any package
#'   input or model object is allowed, object-specific control structures and
#'   training parameters are used for selection and tuning, and objects are
#'   trained sequentially with nested resampling rather than simultaneously with
#'   a global grid.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Model
#'   selection is based on the first calculated metric.
#' @param cutoff argument passed to the \code{metrics} functions.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for model tuning.
#'
#' @return \code{ModelSpecification} class object.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{set_monitor}}, \code{\link{set_optim}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' modelspec <- ModelSpecification(
#'   sale_amount ~ ., data = ICHomes, model = GBMModel
#' )
#' fit(modelspec)
#' }
#'
ModelSpecification <- function(...) {
  UseMethod("ModelSpecification")
}


#' @rdname ModelSpecification-methods
#'
ModelSpecification.default <- function(
  input, model = NULL, control = MachineShop::settings("control"),
  metrics = NULL, cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams"), ...
) {
  object <- new("ModelSpecification",
    input = as.MLInput(input),
    model = as.MLModel(model),
    params = TrainingParams(
      control = control,
      metrics = metrics,
      cutoff = cutoff,
      stat = stat
    ),
    grid = tibble()
  )

  if (is_optim_method(object)) {
    object@grid <- expand_modelgrid(object)
    object@input <- update_slots(object@input, params = list())
    object@model <- update_slots(object@model, params = list())
  }

  object
}


#' @rdname ModelSpecification-methods
#'
ModelSpecification.formula <- function(formula, data, model, ...) {
  ModelSpecification(as.MLInput(formula, data), model = model, ...)
}


#' @rdname ModelSpecification-methods
#'
ModelSpecification.matrix <- function(x, y, model, ...) {
  ModelSpecification(as.MLInput(x, y), model = model, ...)
}


#' @rdname ModelSpecification-methods
#'
ModelSpecification.ModelFrame <- function(input, model = NULL, ...) {
  ModelSpecification.default(input, model = model, ...)
}


#' @rdname ModelSpecification-methods
#'
ModelSpecification.recipe <- function(input, model = NULL, ...) {
  ModelSpecification.default(input, model = model, ...)
}


setMethod("initialize", "ModelSpecification",
  function(.Object, ..., id = make_id("mspec")) {
    callNextMethod(.Object, ..., id = id)
  }
)


#' Update a Model Specification
#'
#' Update the components of a \pkg{MachineShop} model specification.
#'
#' @rdname update
#'
#' @param object model \link[=ModelSpecification]{specification}.
#' @param params list of values to update the corresponding named input and
#'   model objects and their parameters.  See the \code{object} grid for the
#'   names of updatable objects.
#' @param data data frame with which to update data internal to input objects.
#'   The internal data is a processed version of the original and is not
#'   intended to be supplied by users.
#' @param ... arguments passed to other methods.
#'
#' @return Updated \code{ModelSpecification} class object.
#'
#' @noRd
#'
update.ModelSpecification <- function(object, params = NULL, data = NULL, ...) {
  if (is.list(params)) {
    object@input <- update_slots(object@input, params = params)
    object@model <- update_slots(object@model, params = params)
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
    object@grid <- unique_grid(grid_diff(object@grid, params))
    if (is_empty(object@grid)) object@params@optim <- NullOptimization()
  }
  object@input <- update(object@input, data = data)
  object
}


map_slots <- function(
  fun, object, names = c("input", "inputs", "model", "models"), update = FALSE
) {
  slots_res <- NULL
  for (name in intersect(names, slotNames(object))) {
    slot_obj <- slot(object, name)
    if (is(slot_obj, "ListOf")) {
      convert <- function(x) {
        names(x) <- names(slot_obj)
        ListOf(x)
      }
      slot_res <- unlist(map(function(obj) {
        map_slots(fun, obj, names = names, update = update)
      }, unname(slot_obj)), recursive = FALSE)
    } else {
      convert <- identity
      slot_res <- map_slots(fun, slot_obj, names = names, update = update)
    }
    if (update) {
      slot(object, name) <- convert(slot_res)
    } else {
      slots_res <- c(slots_res, slot_res)
    }
  }
  res <- fun(object)
  if (!update) c(structure(list(res), names = object@id), slots_res) else res
}


update_slots <- function(object, params = NULL, ...) {
  convert <- if (is.null(params)) identity else as.list
  map_slots(function(obj) {
    update(obj, params = convert(params[[obj@id]]))
  }, object, update = TRUE, ...)
}
