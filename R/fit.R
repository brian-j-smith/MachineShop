setGeneric("fit", function(object, x, ...) standardGeneric("fit"))


setMethod("fit", c("MLModel", "data.frame"),
  function(object, x, ...) {
    args <- list(formula = formula(x), data = x)
    args$weights <- model.weights(x)
    do.call(object@fit, c(args, object@params))
  }
)

setMethod("fit", c("MLModel", "formula"),
  function(object, x, data, ...) {
    do.call(object@fit, c(list(formula = x, data = data), object@params))
  }
)


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
