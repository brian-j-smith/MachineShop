#' Model Fitting
#' 
#' Fit a model to estimate its parameters from a data set.
#' 
#' @name fit
#' @rdname fit-methods
#' 
#' @param object prediction model object.
#' @param x defined relationship between model predictors and an outcome.  May
#' be a model.frame (data.frame) containing a formula, data, and optionally case
#' weights; a formula; or a recipe.
#' @param ... further arguments passed to other methods.
#' 
#' @seealso \code{\link[stats]{model.frame}}, \code{\link[recipes]{recipe}},
#' \code{\link{predict}}, \code{\link{varimp}}
#' 
setGeneric("fit", function(object, x, ...) standardGeneric("fit"))


#' @rdname fit-methods
#' @aliases fit,MLModel,data.frame-method
#' 
setMethod("fit", c("MLModel", "data.frame"),
  function(object, x, ...) {
    requireModelNamespaces(object@packages)
    args <- list(formula = formula(x), data = x)
    args$weights <- model.weights(x)
    do.call(object@fit, c(args, object@params))
  }
)


#' @rdname fit-methods
#' @aliases fit,MLModel,formula-method
#' 
#' @param data data frame containing observed predictors and outcomes.
#' 
setMethod("fit", c("MLModel", "formula"),
  function(object, x, data, ...) {
    requireModelNamespaces(object@packages)
    args <- list(formula = x, data = data)
    do.call(object@fit, c(args, object@params))
  }
)


#' @rdname fit-methods
#' @aliases fit,MLModel,recipe-method
#' 
setMethod("fit", c("MLModel", "recipe"),
  function(object, x, ...) {
    x <- prep(x, retain = TRUE)
    fit(object, formula(x), juice(x))
  }
)


argsStepAIC <- function(formula, direction, scope) {
  if(is.null(scope$lower)) scope$lower <- ~ 1
  if(is.null(scope$upper)) scope$upper <- formula[-2]
  formula[-2] <- if(direction == "backward") scope$upper else scope$lower
  list(formula = formula, scope = scope)
}
