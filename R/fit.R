setGeneric("fit", function(object, x, ...) standardGeneric("fit"))


setMethod("fit", c("formula", "data.frame"),
  function(object, x, model, ...) {
    fit(model, object, x)
  }
)


setMethod("fit", c("MLModel", "formula"),
  function(object, x, data, ...) {
    do.call(object@fit, c(list(formula = x, data = data), object@params))
  }
)
