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
#' @param ... arguments passed from the generic function to its methods.  The
#'   first argument of each \code{ModeledInput} method is positional and, as
#'   such, must be given first in calls to them.
#' @param object \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param model \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.  Can
#'   be given first followed by any of the variable specifications.
#'
#' @return \code{ModeledFrame} or \code{ModeledRecipe} class object that
#' inherits from \code{ModelFrame} or \code{recipe}.
#'
#' @seealso \code{\link{as.MLModel}}, \code{\link{fit}}, \code{\link{resample}},
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
ModeledInput <- function(...) {
  UseMethod("ModeledInput")
}


#' @rdname ModeledInput-methods
#'
ModeledInput.formula <- function(formula, data, model, ...) {
  ModeledInput(as.MLInput(formula, data), model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.matrix <- function(x, y, model, ...) {
  ModeledInput(as.MLInput(x, y), model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.ModelFrame <- function(object, model, ...) {
  model <- as.MLModel(model)
  switch_class(object,
    "SelectedModelFrame" = {
      inputs <- map(ModeledInput, object@inputs, model = list(model))
      object@inputs <- ListOf(inputs)
      object
    },
    "default" = new("ModeledFrame", as(object, "ModelFrame"), model = model)
  )
}


#' @rdname ModeledInput-methods
#'
ModeledInput.recipe <- function(object, model, ...) {
  model <- as.MLModel(model)
  switch_class(object,
    "SelectedModelRecipe" = {
      inputs <- map(ModeledInput, object@inputs, model = list(model))
      object@inputs <- ListOf(inputs)
      object
    },
    "TunedModelRecipe" = new("TunedModeledRecipe", object, model = model),
    "default" = new("ModeledRecipe", as(object, "ModelRecipe"), model = model)
  )
}


#' @rdname ModeledInput-methods
#'
ModeledInput.MLModel <- function(model, ...) {
  ModeledInput(..., model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.MLModelFunction <- function(model, ...) {
  ModeledInput(as.MLModel(model), ...)
}


.fit.ModeledFrame <- function(object, ...) {
  fit(as(object, "ModelFrame"), model = object@model)
}


.fit.ModeledRecipe <- function(object, ...) {
  fit(as(object, "ModelRecipe"), model = object@model)
}


.fit.TunedModeledRecipe <- function(object, ...) {
  fit(as(object, "TunedModelRecipe"), model = object@model)
}
