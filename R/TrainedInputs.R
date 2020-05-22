#' Selected Model Inputs
#'
#' Formula, design matrix, model frame, or recipe selection from a candidate
#' set.
#'
#' @aliases SelectedModelFrame
#' @aliases SelectedModelRecipe
#' @rdname SelectedInput
#'
#' @param ... \link{inputs} specifying relationships between model predictor
#'   and response variables.  Supplied inputs must all be of the same type and
#'   may be named or unnamed.
#' @param x list of inputs followed by arguments passed to their method
#'   function.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} or an object that can be converted
#'   to one.
#' @param control \link[=controls]{control} function, function name, or call
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
#' library(MASS)
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
SelectedInput.formula <- function(..., data,
                                  control = MachineShop::settings("control"),
                                  metrics = NULL,
                                  stat = MachineShop::settings("stat.train"),
                                  cutoff = MachineShop::settings("cutoff")) {
  inputs <- list(...)
  if (!all(map_logi(is, inputs, "formula"))) stop("inputs must be formulas")
  mf_list <- map(function(x) {
    ModelFrame(x, data, na.rm = FALSE, strata = strata(response(x, data)))
  }, inputs)
  SelectedInput(mf_list, control = control, metrics = metrics, stat = stat,
                cutoff = cutoff)
}


#' @rdname SelectedInput
#'
SelectedInput.matrix <- function(..., y,
                                 control = MachineShop::settings("control"),
                                 metrics = NULL,
                                 stat = MachineShop::settings("stat.train"),
                                 cutoff = MachineShop::settings("cutoff")) {
  inputs <- list(...)
  if (!all(map_logi(is, inputs, "matrix"))) stop("inputs must be matrices")
  mf_list <- map(ModelFrame,
                 inputs, list(y), na.rm = FALSE, strata = list(strata(y)))
  SelectedInput(mf_list, control = control, metrics = metrics, stat = stat,
                cutoff = cutoff)
}


#' @rdname SelectedInput
#'
SelectedInput.ModelFrame <- function(...,
                                     control = MachineShop::settings("control"),
                                     metrics = NULL,
                                     stat = MachineShop::settings("stat.train"),
                                     cutoff = MachineShop::settings("cutoff")) {

  inputs <- list(...)

  input_classes <- map_chr(function(x) class(x)[1], inputs)
  if (!all(input_classes %in% c("ModelFrame", "ModeledFrame"))) {
    stop("inputs must be ModelFrames or ModeledFrames")
  }

  if (!identical_elements(inputs, function(x) x[[1]])) {
    stop("ModelFrames have different response variables")
  }

  names(inputs) <- make_list_names(inputs, "ModelFrame")
  data <- NULL
  for (i in seq(inputs)) {
    data <- combine_dataframes(as.data.frame(inputs[[i]]), data)
  }

  new("SelectedModelFrame", ModelFrame(data),
      inputs = ListOf(map(terms, inputs)),
      params = list(control = getMLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff))

}


#' @rdname SelectedInput
#'
SelectedInput.recipe <- function(...,
                                 control = MachineShop::settings("control"),
                                 metrics = NULL,
                                 stat = MachineShop::settings("stat.train"),
                                 cutoff = MachineShop::settings("cutoff")) {

  inputs <- list(...)

  for (i in seq(inputs)) inputs[[i]] <- ModelRecipe(inputs[[i]])

  get_info <- function(x) {
    info <- summary(x)
    info <- info[info$role != "predictor", ]
    info_order <- do.call(order, info)
    info[info_order, ]
  }
  if (!identical_elements(inputs, get_info)) {
    stop("recipes have different non-predictor variables")
  }
  info <- get_info(inputs[[1]])

  names(inputs) <- make_list_names(inputs, "Recipe")
  data <- NULL
  for (i in seq(inputs)) {
    data <- combine_dataframes(as.data.frame(inputs[[i]]), data)
    inputs[[i]] <- recipe(inputs[[i]], tibble())
  }

  outcome_vars <- info$variable[info$role == "outcome"]
  fo <- reformulate(".", paste(outcome_vars, collapse = "+"))
  new("SelectedModelRecipe", new("ModelRecipe", recipe(fo, data = data)),
      inputs = ListOf(inputs),
      params = list(control = getMLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff))

}


#' @rdname SelectedInput
#'
SelectedInput.list <- function(x, ...) {
  do.call(SelectedInput, c(x, list(...)))
}


.fit.SelectedInput <- function(x, ...) {
  inputs <- x@inputs
  input_class <- class(x)
  switch(input_class,
    SelectedModelFrame = {
      grid_name <- "ModelFrame"
      object <- as(x, "ModelFrame")
      set_input <- function(x) {
        input <- structure(object, terms = terms(x))
        if (is(x, "ModeledTerms")) {
          ModeledInput(input, model = x@model)
        } else input
      }
    },
    SelectedModelRecipe = {
      grid_name <- "ModelRecipe"
      object <- as.data.frame(x)
      set_input <- function(x) recipe(x, object[unique(summary(x)$variable)])
    },
    stop("unsupported input object of class ", input_class)
  )
  trainbit <- resample_selection(inputs, set_input, x@params, ...,
                                 class = "SelectedInput")
  trainbit$grid <- tibble(Input = factor(seq(inputs)))
  names(trainbit$grid) <- grid_name
  input <- set_input(inputs[[trainbit$selected]])
  push(do.call(TrainBit, trainbit), fit(input, ...))
}


#' Tuned Model Inputs
#'
#' Recipe tuning over a grid of parameter values.
#'
#' @aliases TunedModelRecipe
#'
#' @rdname TunedInput
#'
#' @param x untrained \code{\link[recipes]{recipe}}.
#' @param grid \code{RecipeGrid} containing parameter values at which to
#'   evaluate a recipe, such as those returned by \code{\link{expand_steps}}.
#' @param control \link[=controls]{control} function, function name, or call
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
#' library(MASS)
#'
#' rec <- recipe(medv ~ ., data = Boston) %>%
#'   step_pca(all_numeric(), -all_outcomes(), id = "pca")
#'
#' grid <- expand_steps(
#'   pca = list(num_comp = 1:3)
#' )
#'
#' fit(TunedInput(rec, grid = grid), model = GLMModel)
#'
TunedInput <- function(x, ...) {
  UseMethod("TunedInput")
}


#' @rdname TunedInput
#'
TunedInput.recipe <- function(x, grid = expand_steps(),
                              control = MachineShop::settings("control"),
                              metrics = NULL,
                              stat = MachineShop::settings("stat.train"),
                              cutoff = MachineShop::settings("cutoff"), ...) {

  object <- new("TunedModelRecipe", ModelRecipe(x),
                grid = grid,
                params = list(control = getMLObject(control, "MLControl"),
                              metrics = metrics, stat = stat, cutoff = cutoff))

  grid_names <- names(object@grid)
  step_ids <- map_chr(getElement, object$steps, "id")
  found <- grid_names %in% step_ids
  if (!all(found)) {
    missing <- grid_names[!found]
    stop(label_items("grid step name", missing),
         label_items("; not found in recipe step id", step_ids))
  }

  if (is(x, "ModeledRecipe")) {
    new("TunedModeledRecipe", object, model = x@model)
  } else object

}


.fit.TunedModelRecipe <- function(x, model, ...) {
  grid <- x@grid
  recipe <- as(x, "ModelRecipe")
  if (all(dim(grid) != 0)) {
    grid_split <- split(grid, 1:nrow(grid))
    set_input <- function(x) do.call(update, c(list(recipe), x))
    trainbit <- resample_selection(grid_split, set_input, x@params, model,
                                   class = "TunedInput")
    trainbit$grid <- tibble(ModelRecipe = asS3(grid))
    input <- set_input(grid_split[[trainbit$selected]])
    push(do.call(TrainBit, trainbit), fit(input, model = model))
  } else {
    fit(recipe, model = model)
  }
}
