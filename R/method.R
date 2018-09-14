setGeneric("method", function(object, ...) standardGeneric("method"))


setMethod("method", "MLControl",
  function(object, ...) "Custom Resampling Method"
)


setMethod("method", "BootControl",
  function(object, ...) "Boot"
)


setMethod("method", "CVControl",
  function(object, ...) {
    paste0(ifelse(object@repeats > 1, "Repeated ", ""),
           object@folds, "-Fold CV")
  }
)


setMethod("method", "OOBControl",
  function(object, ...) "OOB"
)
