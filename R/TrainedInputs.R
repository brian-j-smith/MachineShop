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
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe selection.
#' @param cutoff argument passed to the \code{metrics} functions.
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
  stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff")
) {
  inputs <- list(...)
  if (!all(map_logi(is, inputs, "formula"))) {
    throw(Error("inputs must be formulas"))
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
  stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff")
) {
  inputs <- list(...)
  if (!all(map_logi(is, inputs, "matrix"))) {
    throw(Error("inputs must be matrices"))
  }
  mf_list <- map(ModelFrame, inputs, list(y), strata = list(y), na.rm = FALSE)
  SelectedInput(mf_list, control = control, metrics = metrics, stat = stat,
                cutoff = cutoff)
}


#' @rdname SelectedInput
#'
SelectedInput.ModelFrame <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff")
) {

  inputs <- list(...)

  input_classes <- map_chr(function(x) class1(x), inputs)
  if (!all(input_classes %in% c("ModelFrame", "ModeledFrame"))) {
    throw(Error("inputs must be ModelFrames or ModeledFrames"))
  }

  if (!identical_elements(inputs, function(x) x[[1]])) {
    throw(Error("ModelFrames have different response variables"))
  }

  names(inputs) <- make_list_names(inputs, "ModelFrame")
  data <- NULL
  for (i in seq_along(inputs)) {
    data <- combine_data_frames(as.data.frame(inputs[[i]]), data)
  }

  new("SelectedModelFrame", ModelFrame(data),
      inputs = ListOf(map(terms, inputs)),
      params = list(control = as.MLControl(control), metrics = metrics,
                    stat = stat, cutoff = cutoff))

}


#' @rdname SelectedInput
#'
SelectedInput.recipe <- function(
  ..., control = MachineShop::settings("control"), metrics = NULL,
  stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff")
) {

  inputs <- list(...)

  for (i in seq_along(inputs)) inputs[[i]] <- ModelRecipe(inputs[[i]])

  get_info <- function(x, roles = NULL, exclude = FALSE) {
    info <- summary(x)
    keep <- if (length(roles)) info$role %in% roles else TRUE
    if (exclude) keep <- !keep
    info <- info[keep, ]
    info_order <- do.call(order, info)
    info[info_order, ]
  }
  common_info <- function(x) get_info(x, roles = "predictor", exclude = TRUE)
  if (!identical_elements(inputs, common_info)) {
    throw(Error("recipes have different non-predictor variables"))
  }

  names(inputs) <- make_list_names(inputs, "Recipe")
  data <- NULL
  for (i in seq_along(inputs)) {
    data <- combine_data_frames(as.data.frame(inputs[[i]]), data)
    inputs[[i]] <- recipe(inputs[[i]], tibble())
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
      params = list(control = as.MLControl(control), metrics = metrics,
                    stat = stat, cutoff = cutoff))

}


#' @rdname SelectedInput
#'
SelectedInput.list <- function(x, ...) {
  do.call(SelectedInput, c(x, list(...)))
}


.fit.SelectedInput <- function(object, ...) {
  inputs <- object@inputs
  params <- object@params
  update_input <- switch(class(object),
    "SelectedModelFrame" = {
      object <- as(object, "ModelFrame")
      function(x) {
        input <- structure(object, terms = terms(x))
        if (is(x, "ModeledTerms")) {
          ModeledInput(input, model = x@model)
        } else input
      }
    },
    "SelectedModelRecipe" = {
      object <- as.data.frame(object)
      function(x) recipe(x, object[unique(summary(x)$variable)])
    },
    TypeError(object, c("SelectedModelFrame", "SelectedModelRecipe"), "input")
  )
  throw(update_input)
  train_step <- resample_selection(inputs, update_input, params, ...,
                                   name = "SelectedInput")
  train_step@grid$params <- factor(seq_along(inputs))
  selected <- which(train_step@grid$selected)
  input <- update_input(inputs[[selected]])
  push(train_step, fit(input, ...))
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
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe tuning.
#' @param cutoff argument passed to the \code{metrics} functions.
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
  metrics = NULL, stat = MachineShop::settings("stat.Trained"),
  cutoff = MachineShop::settings("cutoff"), ...
) {

  object <- new("TunedModelRecipe", ModelRecipe(object),
                grid = grid,
                params = list(control = as.MLControl(control),
                              metrics = metrics, stat = stat, cutoff = cutoff))

  grid_names <- names(object@grid)
  step_ids <- map_chr(getElement, object$steps, "id")
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


.fit.TunedModelRecipe <- function(object, model, ...) {
  grid <- object@grid
  recipe <- as(object, "ModelRecipe")
  if (all(size(grid) > 0)) {
    grid_split <- split(grid, seq_len(nrow(grid)))
    update_input <- function(x) do.call(update, c(list(recipe), x))
    train_step <- resample_selection(grid_split, update_input, object@params,
                                     model, name = "TunedInput")
    train_step@grid$params <- asS3(grid)
    selected <- which(train_step@grid$selected)
    input <- update_input(grid_split[[selected]])
    push(train_step, fit(input, model = model))
  } else {
    fit(recipe, model = model)
  }
}
