#' Selected Model Inputs
#'
#' Formula, design matrix, model frame, or recipe selection from a candidate
#' set.
#'
#' @aliases SelectedModeledFrame
#' @aliases SelectedModeledRecipe
#' @rdname SelectedInput
#'
#' @param x,... \link{inputs} specifying relationships between model predictor
#'   and response variables, or a list of these.
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
#' @return \code{SelectedModelFrame}, \code{SelectedModelRecipe},
#' \code{SelectedModeledFrame}, or \code{SelectedModeledRecipe} class object
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
SelectedInput <- function(x, ...) {
  if (missing(x) && !missing(..1)) {
    UseMethod("SelectedInput", ..1)
  } else {
    UseMethod("SelectedInput")
  }
}


#' @rdname SelectedInput
#'
SelectedInput.formula <- function(x, ..., data,
                                  control = MachineShop::settings("control"),
                                  metrics = NULL,
                                  stat = MachineShop::settings("stat.train"),
                                  cutoff = MachineShop::settings("cutoff")) {
  inputs <- if (missing(x)) list(...) else list(x, ...)
  if (!all(map_logi(is, inputs, "formula"))) stop("inputs must be formulas")
  SelectedInput(map(ModelFrame, inputs, list(data),
                    MoreArgs = list(na.rm = FALSE)),
                control = control, metrics = metrics, stat = stat,
                cutoff = cutoff)
}


#' @rdname SelectedInput
#'
SelectedInput.matrix <- function(x, ..., y,
                                 control = MachineShop::settings("control"),
                                 metrics = NULL,
                                 stat = MachineShop::settings("stat.train"),
                                 cutoff = MachineShop::settings("cutoff")) {
  inputs <- if (missing(x)) list(...) else list(x, ...)
  if (!all(map_logi(is, inputs, "matrix"))) stop("inputs must be matrices")
  SelectedInput(map(ModelFrame, inputs, list(y),
                    MoreArgs = list(na.rm = FALSE)),
                control = control, metrics = metrics, stat = stat,
                cutoff = cutoff)
}


#' @rdname SelectedInput
#'
SelectedInput.ModelFrame <- function(x, ...,
                                     control = MachineShop::settings("control"),
                                     metrics = NULL,
                                     stat = MachineShop::settings("stat.train"),
                                     cutoff = MachineShop::settings("cutoff")) {

  inputs <- if (missing(x)) list(...) else list(x, ...)

  if (!all(map_logi(is, inputs, "ModelFrame"))) {
    stop("inputs must be ModelFrames")
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
      inputs = ListOf(map(attr, inputs, "terms")),
      params = list(control = getMLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff))

}


#' @rdname SelectedInput
#'
SelectedInput.recipe <- function(x, ...,
                                 control = MachineShop::settings("control"),
                                 metrics = NULL,
                                 stat = MachineShop::settings("stat.train"),
                                 cutoff = MachineShop::settings("cutoff")) {

  inputs <- if (missing(x)) list(...) else list(x, ...)

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
SelectedInput.ModeledInput <-
  function(x, ...,
           control = MachineShop::settings("control"),
           metrics = NULL,
           stat = MachineShop::settings("stat.train"),
           cutoff = MachineShop::settings("cutoff")) {

    inputs <- if (missing(x)) list(...) else list(x, ...)

    if (!all(map_logi(is, inputs, "ModeledInput"))) {
      stop("inputs must be ModeledInputs")
    }

    if (!identical_elements(inputs, function(x) class(x))) {
      stop("inputs are of different ModeledInput types")
    }

    models <- map(slot, inputs, "model")
    input_class <- class(inputs[[1]])
    switch(input_class,
           ModeledFrame = {
             to_class <- "ModelFrame"
             object_class <- "SelectedModeledFrame"
           },
           ModeledRecipe = {
             to_class <- "ModelRecipe"
             object_class <- "SelectedModeledRecipe"
           })
    inputs <- map(as, inputs, to_class)
    names(inputs) <- make_list_names(inputs, input_class)

    args <- c(inputs, control = control, metrics = metrics, stat = stat,
              cutoff = cutoff)
    object <- do.call(SelectedInput, args)

    new(object_class, as(object, to_class),
        inputs = ListOf(map(list, input = object@inputs, model = models)),
        params = object@params)

  }


#' @rdname SelectedInput
#'
SelectedInput.list <- function(x, ...) {
  do.call(SelectedInput, c(x, list(...)))
}


.fit.SelectedInput <- function(x, ...) {
  inputs <- x@inputs
  switch(class(x),
    SelectedModelFrame = {
      input_class <- "ModelFrame"
      object <- as(x, "ModelFrame")
      set_input <- function(x) structure(object, terms = x)
    },
    SelectedModeledFrame = {
      input_class <- "ModeledFrame"
      object <- as(x, "ModelFrame")
      set_input <- function(x) {
        ModeledInput(structure(object, terms = x$input), model = x$model)
      }
    },
    SelectedModelRecipe = {
      input_class <- "ModelRecipe"
      object <- as.data.frame(x)
      set_input <- function(x) recipe(x, object[unique(summary(x)$variable)])
    },
    SelectedModeledRecipe = {
      input_class <- "ModeledRecipe"
      object <- as.data.frame(x)
      set_input <- function(x) {
        ModeledInput(recipe(x$input, object[unique(summary(x$input)$variable)]),
                     model = x$model)
      }
    }
  )
  trainbit <- resample_selection(inputs, set_input, x@params, ...)
  trainbit$grid <- tibble(Input = factor(seq(inputs)))
  names(trainbit$grid) <- input_class
  input <- set_input(inputs[[trainbit$selected]])
  push(do.call(TrainBit, trainbit), fit(input, ...))
}


#' Tuned Model Inputs
#'
#' Recipe tuning over a grid of parameter values.
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

  object

}


.fit.TunedModelRecipe <- function(x, model, ...) {
  grid <- x@grid
  recipe <- as(x, "ModelRecipe")
  if (all(dim(grid) != 0)) {
    grid_split <- split(grid, 1:nrow(grid))
    set_input <- function(x) update(recipe, x)
    trainbit <- resample_selection(grid_split, set_input, x@params, model)
    trainbit$grid <- tibble(ModelRecipe = grid)
    input <- set_input(grid_split[[trainbit$selected]])
    push(do.call(TrainBit, trainbit), fit(input, model = model))
  } else {
    fit(recipe, model = model)
  }
}
