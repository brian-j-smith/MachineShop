fit <- function(formula, data, model) {
  .fit(model, formula, data)
}


setGeneric(".fit", function(object, ...) standardGeneric(".fit"))


lapply(.modelNames, function(signature) {
  setMethod(".fit", signature,
    function(object, formula, data) {
      object$fit(formula, data)
    }
  )
})
