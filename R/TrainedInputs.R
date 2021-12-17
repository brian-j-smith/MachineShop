#' Selected Model Inputs
#'
#' Formula, design matrix, model frame, or recipe selection from a candidate
#' set.
#'
#' @aliases SelectedModelFrame
#' @aliases SelectedModelRecipe
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
#' @return \code{SelectedModelFrame} or \code{SelectedModelRecipe} class object
#' that inherits from \code{SelectedInput} and \code{ModelFrame} or
#' \code{recipe}.
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
    throw(Error("Inputs must be formulas."))
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
    throw(Error("Inputs must be matrices."))
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

  inputs <- list(...)

  if (length(inputs) == 1) {
    return(inputs[[1]])
  }

  input_classes <- map("char", function(x) class1(x), inputs)
  if (!all(input_classes %in% c("ModelFrame", "ModeledFrame"))) {
    throw(Error("Inputs must be ModelFrames or ModeledFrames."))
  }

  if (!identical_elements(inputs, function(x) x[[1]])) {
    throw(Error("ModelFrames have different response variables."))
  }

  default_names <- map("char", class, inputs)
  names(inputs) <- make_names_along(inputs, default_names)
  data <- NULL
  for (i in seq_along(inputs)) {
    data <- combine_data_frames(as.data.frame(inputs[[i]]), data)
  }

  new("SelectedModelFrame", ModelFrame(data),
    inputs = ListOf(map(terms, inputs)),
    params = TrainingParams(
      control = control,
      metrics = metrics,
      cutoff = cutoff,
      stat = stat
    )
  )

}


#' @rdname SelectedInput
#'
SelectedInput.recipe <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  cutoff = MachineShop::settings("cutoff"),
  stat = MachineShop::settings("stat.TrainingParams")
) {

  inputs <- list(...)
  for (i in seq_along(inputs)) inputs[[i]] <- ModelRecipe(inputs[[i]])

  if (length(inputs) == 1) {
    return(inputs[[1]])
  }

  get_info <- function(x, roles = character(), exclude = FALSE) {
    info <- summary(x)
    keep <- if (length(roles)) info$role %in% roles else TRUE
    if (exclude) keep <- !keep
    info <- info[keep, ]
    info_order <- do.call(order, info)
    info[info_order, ]
  }
  common_info <- function(x) get_info(x, roles = "predictor", exclude = TRUE)
  if (!identical_elements(inputs, common_info)) {
    throw(Error("Recipes have different non-predictor variables."))
  }

  default_names <- map("char", class, inputs)
  names(inputs) <- make_names_along(inputs, default_names)
  data <- NULL
  for (i in seq_along(inputs)) {
    data <- combine_data_frames(as.data.frame(inputs[[i]]), data)
    inputs[[i]] <- update(inputs[[i]], data = tibble(.rows = nrow(data)))
  }

  outcome_vars <- get_info(inputs[[1]], roles = "outcome")$variable
  fo <- reformulate(".", paste(outcome_vars, collapse = "+"))
  rec <- recipe(fo, data = data)

  other_vars <- get_info(inputs[[1]], roles = c("predictor", "outcome"),
                         exclude = TRUE)$variable
  if (length(other_vars)) {
    args <- list(rec, other_vars, new_role = "other")
    rec <- do.call(recipes::update_role, args)
  }

  new("SelectedModelRecipe", new("ModelRecipe", rec),
    inputs = ListOf(inputs),
    params = TrainingParams(
      control = control,
      metrics = metrics,
      cutoff = cutoff,
      stat = stat
    )
  )

}


#' @rdname SelectedInput
#'
SelectedInput.list <- function(x, ...) {
  do.call(SelectedInput, c(x, list(...)))
}


.fit.SelectedInput <- function(object, ...) {
  fit_grid(object, ...)
}


update.SelectedInput <- function(object, params = list(), ...) {
  object <- as(object, "SelectedInput")
  new_params <- as(object, "list")
  new_params[names(params)] <- params
  objects <- new_params$objects
  new_params[c("objects", "id")] <- NULL
  res <- do.call(SelectedInput, c(objects, new_params))
  res@id <- object@id
  res
}


update.SelectedModelFrame <- function(object, params = list(), ...) {
  object <- subset_selected(object, "inputs", params$id)
  mf <- as(object, "ModelFrame")
  object@inputs <- ListOf(map(function(x) {
    if (is(x, "ModeledTerms")) mf <- ModeledInput(mf, model = x@model)
    mf@id <- x@id
    attr(mf, "terms") <- terms(x)
    mf
  }, object@inputs))
  NextMethod()
}


update.SelectedModelRecipe <- function(object, params = list(), ...) {
  object <- subset_selected(object, "inputs", params$id)
  data <- as.data.frame(object)
  object@inputs <- ListOf(map(function(x) {
    rec <- ModelRecipe(update(x, data = data[unique(summary(x)$variable)]))
    rec@id <- x@id
    rec
  }, object@inputs))
  NextMethod()
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
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' library(recipes)
#' data(Boston, package = "MASS")
#'
#' rec <- recipe(medv ~ ., data = Boston) %>%
#'   step_pca(all_numeric(), -all_outcomes(), id = "pca")
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

  if (is(object, "ModeledRecipe")) {
    new("TunedModeledRecipe", object, model = object@model)
  } else object

}


.fit.TunedModelRecipe <- function(object, ...) {
  fit_grid(object, ...)
}


update.TunedModelRecipe <- function(
  object, params = NULL, ...
) {
  if (is.list(params)) {
    update(as(object, "ModelRecipe"), params = params, new_id = object@id)
  } else {
    object
  }
}
