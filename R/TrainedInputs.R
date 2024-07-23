#' Selected Model Inputs
#'
#' Formula, design matrix, model frame, or recipe selection from a candidate
#' set.
#'
#' @aliases SelectedModelFrame
#' @aliases SelectedModelRecipe
#' @aliases SelectedModelSpecification
#' @rdname SelectedInput
#'
#' @param ... \link{inputs} defining relationships between model predictor
#'   and response variables.  Supplied inputs must all be of the same type and
#'   may be named or unnamed.
#' @param x list of inputs followed by arguments passed to their method
#'   function.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing predictor and response
#'   variables.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Recipe
#'   selection is based on the first calculated metric.
#' @param cutoff argument passed to the \code{metrics} functions.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe selection.
#'
#' @return \code{SelectedModelFrame}, \code{SelectedModelRecipe}, or
#' \code{SelectedModelSpecification} class object that inherits from
#' \code{SelectedInput} and \code{ModelFrame}, \code{recipe}, or
#' \code{ModelSpecification}, respectively.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' ## Selected model frame
#' sel_mf <- SelectedInput(
#'   sale_amount ~ sale_year + built + style + construction,
#'   sale_amount ~ sale_year + base_size + bedrooms + basement,
#'   data = ICHomes
#' )
#'
#' fit(sel_mf, model = GLMModel)
#'
#' ## Selected recipe
#' library(recipes)
#' data(Boston, package = "MASS")
#'
#' rec1 <- recipe(medv ~ crim + zn + indus + chas + nox + rm, data = Boston)
#' rec2 <- recipe(medv ~ chas + nox + rm + age + dis + rad + tax, data = Boston)
#' sel_rec <- SelectedInput(rec1, rec2)
#'
#' fit(sel_rec, model = GLMModel)
#'
SelectedInput <- function(...) {
  UseMethod("SelectedInput")
}


#' @rdname SelectedInput
#'
SelectedInput.formula <- function(
  ..., data, control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {
  inputs <- list(...)
  if (!all(map("logi", is, inputs, "formula"))) {
    throw(Error("Inputs must all be formulas."))
  }
  mf_list <- map(function(x) {
    do.call(ModelFrame, list(x, data, strata = response(x), na.rm = FALSE))
  }, inputs)
  SelectedInput(mf_list, control = control, metrics = metrics, stat = stat,
                cutoff = cutoff)
}


#' @rdname SelectedInput
#'
SelectedInput.matrix <- function(
  ..., y, control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {
  inputs <- list(...)
  if (!all(map("logi", is, inputs, "matrix"))) {
    throw(Error("Inputs must all be matrices."))
  }
  mf_list <- map(ModelFrame, inputs, list(y), strata = list(y), na.rm = FALSE)
  SelectedInput(mf_list, control = control, metrics = metrics, stat = stat,
                cutoff = cutoff)
}


#' @rdname SelectedInput
#'
SelectedInput.ModelFrame <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {

  if (...length() == 1) return(..1)

  params <- do.call(TrainingParams, as.list(environment()))
  inputs <- list(...)

  if (!all(map("char", class1, inputs) == "ModelFrame")) {
    throw(Error("Inputs must all be ModelFrames."))
  }

  if (!identical_elements(inputs, function(x) x[[1]])) {
    throw(Error("ModelFrames have different response variables."))
  }

  combined <- combine_inputs(inputs)

  new("SelectedModelFrame", ModelFrame(combined$data),
    candidates = combined$candidates,
    params = params
  )

}


#' @rdname SelectedInput
#'
SelectedInput.recipe <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {

  if (...length() == 1) return(..1)

  params <- do.call(TrainingParams, as.list(environment()))
  inputs <- list(...)
  for (i in seq_along(inputs)) inputs[[i]] <- ModelRecipe(inputs[[i]])

  get_info <- function(x) {
    info <- summary(x)
    info <- info[info$role != "predictor", ]
    info[do.call(order, info[c("variable", "role", "source")]), ]
  }
  if (!identical_elements(inputs, get_info)) {
    throw(Error("Recipes have different non-predictor variables."))
  }

  combined <- combine_inputs(inputs)

  info <- summary(combined$candidates[[1]])
  outcomes <- info$variable[info$role == "outcome"]
  others <- info$variable[!(info$role %in% c("outcome", "predictor"))]
  fo <- reformulate(".", paste(outcomes, collapse = "+"))
  rec <- recipe(fo, data = combined$data)
  if (length(others)) {
    args <- list(rec, others, new_role = "other")
    rec <- do.call(recipes::update_role, args)
  }

  new("SelectedModelRecipe", new("ModelRecipe", rec),
    candidates = combined$candidates,
    params = params
  )

}


#' @rdname SelectedInput
#'
SelectedInput.ModelSpecification <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {

  if (...length() == 1) return(..1)

  params <- do.call(TrainingParams, as.list(environment()))
  modelspecs <- list(...)
  default_names <- map("char", class, modelspecs)
  names(modelspecs) <- make_names_along(modelspecs, default_names)

  if (any(map("char", class1, modelspecs) != "ModelSpecification")) {
    throw(Error("Inputs must all be ModelSpecifications."))
  }

  sel_input <- SelectedInput(map(slot, modelspecs, "input"))
  classes <- c("ModelFrame", "ModelRecipe")
  input <- as(sel_input, classes[map("logi", is, list(sel_input), classes)])
  modelspecs <- map(function(modelspec, input) {
    update(modelspec, data = as.data.frame(input))
  }, modelspecs, sel_input@candidates)

  new("SelectedModelSpecification", ModelSpecification(input, NullModel()),
    candidates = ListOf(modelspecs),
    params = params
  )

}


#' @rdname SelectedInput
#'
SelectedInput.list <- function(x, ...) {
  do.call(SelectedInput, c(x, list(...)))
}


.fit.SelectedInput <- function(object, ...) {
  .fit_optim(object, ...)
}


update.SelectedInput <- function(object, params = NULL, ...) {
  if (is.list(params)) {
    id <- object@id
    object <- subset_selected(object, "candidates", params$id)
    data <- as.data.frame(object)
    object@candidates <- ListOf(map(function(input) {
      res <- update(input, data = data[names(as.data.frame(input))])
      res@id <- input@id
      res
    }, object@candidates))
    new_params <- as(as(object, "SelectedInput"), "list")
    new_params[names(params)] <- params
    candidates <- new_params$candidates
    new_params[c("candidates", "id")] <- NULL
    object <- do.call(SelectedInput, c(candidates, new_params))
    object@id <- id
  }
  NextMethod(params = NULL, check_grid = FALSE)
}


#' Tuned Model Inputs
#'
#' Recipe tuning over a grid of parameter values.
#'
#' @aliases TunedModelRecipe
#'
#' @rdname TunedInput
#'
#' @param object untrained \code{\link[recipes]{recipe}}.
#' @param grid \code{RecipeGrid} containing parameter values at which to
#'   evaluate a recipe, such as those returned by \code{\link{expand_steps}}.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Recipe
#'   selection is based on the first calculated metric.
#' @param cutoff argument passed to the \code{metrics} functions.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe tuning.
#' @param ... arguments passed to other methods.
#'
#' @return \code{TunedModelRecipe} class object that inherits from
#' \code{TunedInput} and \code{recipe}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{set_optim}}
#'
#' @examples
#' library(recipes)
#' data(Boston, package = "MASS")
#'
#' rec <- recipe(medv ~ ., data = Boston) %>%
#'   step_pca(all_numeric_predictors(), id = "pca")
#'
#' grid <- expand_steps(
#'   pca = list(num_comp = 1:2)
#' )
#'
#' fit(TunedInput(rec, grid = grid), model = GLMModel)
#'
TunedInput <- function(object, ...) {
  UseMethod("TunedInput")
}


#' @rdname TunedInput
#'
TunedInput.recipe <- function(
  object, grid = expand_steps(), control = MachineShop::settings("control"),
  metrics = NULL, cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams"), ...
) {

  object <- new("TunedModelRecipe", ModelRecipe(object),
    grid = grid,
    params = TrainingParams(
      control = control,
      metrics = metrics,
      cutoff = cutoff,
      stat = stat
    )
  )

  grid_names <- names(object@grid)
  step_ids <- map("char", getElement, object$steps, "id")
  missing <- !(grid_names %in% step_ids)
  if (any(missing)) {
    throw(Error(
      note_items("Grid step name{?s}: ", grid_names[missing], "; "),
      note_items("not found in recipe step id{?s}: ", step_ids, ".")
    ))
  }

  object

}


.fit.TunedModelRecipe <- function(object, ...) {
  .fit_optim(object, ...)
}


update.TunedModelRecipe <- function(object, params = NULL, ...) {
  if (is.list(params)) {
    update(as(object, "ModelRecipe"), params = params)
  } else {
    object
  }
}
