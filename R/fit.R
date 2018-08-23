fit <- function(formula, data, model) {
  .fit(model, formula, data)
}


setGeneric(".fit", function(object, x, ...) standardGeneric(".fit"))


setMethod(".fit", c("AbstractModel", "formula"),
  function(object, x, data) {
    do.call(object@fit, c(list(formula = x, data = data), object@params))
  }
)
