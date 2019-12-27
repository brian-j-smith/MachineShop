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
#' @param x input specified as a \code{\link{formula}}, design
#'   \code{\link{matrix}} of predictors, \code{\link{ModelFrame}}, or untrained
#'   \code{\link[recipes]{recipe}}.  Alternatively, a \link[=models]{model}
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
  ModeledFrame(ModelFrame(x, data), model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.matrix <- function(x, y, model, ...) {
  ModeledFrame(ModelFrame(x, y), model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.ModelFrame <- function(x, model, ...) {
  ModeledFrame(x, model = model)
}


#' @rdname ModeledInput-methods
#'
ModeledInput.recipe <- function(x, model, ...) {
  ModeledRecipe(ModelRecipe(x), model = model)
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


ModeledFrame <- function(object, ..., model) {
  new("ModeledFrame", object, model = getMLObject(model, "MLModel"))
}


ModeledRecipe <- function(object, ..., model) {
  new("ModeledRecipe", object, model = getMLObject(model, "MLModel"))
}
