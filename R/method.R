setGeneric("method", function(object, ...) standardGeneric("method"))


setMethod("method", "MLControl",
  function(object, ...) "Custom Resampling Method"
)


setMethod("method", "BootMLControl",
  function(object, ...) "Boot"
)


setMethod("method", "CVMLControl",
  function(object, ...) {
    paste0(ifelse(object@repeats > 1, "Repeated ", ""),
           object@folds, "-Fold CV")
  }
)


setMethod("method", "OOBMLControl",
  function(object, ...) "OOB"
)


setMethod("method", "SplitMLControl",
  function(object, ...) "Split"
)


setMethod("method", "TrainMLControl",
  function(object, ...) "Training Resubstitution"
)
