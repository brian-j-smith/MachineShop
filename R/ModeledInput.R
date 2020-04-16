#' ModeledInput Classes
#'
#' Class for storing a model input and specification pair for \pkg{MachineShop}
#' model fitting.
#'
#' @name ModeledInput
#' @aliases ModeledFrame
#' @aliases ModeledRecipe
#' @rdname ModeledInput-methods
#'
#' @param x \link[=inputs]{input} specifying a relationship between model
#'   predictor and response variables.  Alternatively, a \link[=models]{model}
#'   function or call may be given first followed by the input specification.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} or an object that can be converted
#'   to one.
#' @param model \link[=models]{model} function, function name, or call.
#' @param ... arguments passed to other methods.
#'
#' @return \code{ModeledFrame} or \code{ModeledRecipe} class object that
#' inherits from \code{ModelFrame} or \code{recipe}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{SelectedInput}}
#'
#' @examples
#' ## Modeled model frame
#' mod_mf <- ModeledInput(sale_amount ~ ., data = ICHomes, model = GLMModel)
#' fit(mod_mf)
#'
#' ## Modeled recipe
#' library(recipes)
#'
#' rec <- recipe(sale_amount ~ ., data = ICHomes)
#' mod_rec <- ModeledInput(rec, model = GLMModel)
#' fit(mod_rec)
#'
ModeledInput <- function(x, ...) {
  UseMethod("ModeledInput")
}


#' @rdname ModeledInput-methods
#'
ModeledInput.formula <- function(x, data, model, ...) {
  mf <- ModelFrame(x, data, na.rm = FALSE, strata = strata(response(x, data)))
  ModeledInput(mf, model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.matrix <- function(x, y, model, ...) {
  mf <- ModelFrame(x, y, na.rm = FALSE, strata = strata(y))
  ModeledInput(mf, model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.ModelFrame <- function(x, model, ...) {
  model <- getMLObject(model, "MLModel")
  switch_class(x,
               SelectedModelFrame = {
                 inputs <- map(ModeledInput, x@inputs, model = list(model))
                 x@inputs <- ListOf(inputs)
                 x
               },
               default = {
                 new("ModeledFrame", as(x, "ModelFrame"), model = model)
               })
}


ModeledInput.ModelDesignTerms <- function(x, model, ...) {
  new("ModeledDesignTerms", x, model = model)
}


ModeledInput.ModelFormulaTerms <- function(x, model, ...) {
  new("ModeledFormulaTerms", x, model = model)
}


ModeledInput.ModeledTerms <- function(x, model, ...) {
  x@model <- model
  x
}


#' @rdname ModeledInput-methods
#'
ModeledInput.recipe <- function(x, model, ...) {
  model <- getMLObject(model, "MLModel")
  switch_class(x,
               SelectedModelRecipe = {
                 inputs <- map(ModeledInput, x@inputs, model = list(model))
                 x@inputs <- ListOf(inputs)
                 x
               },
               default = {
                 new("ModeledRecipe", as(x, "ModelRecipe"), model = model)
               })
}


#' @rdname ModeledInput-methods
#'
ModeledInput.MLModel <- function(x, ...) {
  ModeledInput(..., model = x)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.MLModelFunction <- function(x, ...) {
  ModeledInput(x(), ...)
}
