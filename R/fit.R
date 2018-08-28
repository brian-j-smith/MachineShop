setGeneric("fit", function(object, x, ...) standardGeneric("fit"))


setMethod("fit", c("MLModel", "formula"),
  function(object, x, data, ...) {
    do.call(object@fit, c(list(formula = x, data = data), object@params))
  }
)


fitStepAIC <- function(f, data, formula, direction, scope, k, trace, steps) {
  if(is.null(scope$lower)) scope$lower <- ~ 1
  if(is.null(scope$upper)) scope$upper <- formula[-2]
  formula[-2] <- if(direction == "backward") scope$upper else scope$lower
  environment(formula) <- environment()
  f(formula, data) %>%
    MASS::stepAIC(direction = direction, scope = scope, k = k, trace = trace,
                  steps = steps)
}
