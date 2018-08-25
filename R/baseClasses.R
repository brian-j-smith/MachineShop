setClass("MLControl",
  slots = c(summary = "function", cutoff = "numeric", cutoff.index = "function",
            survtimes = "numeric"),
  contains = "VIRTUAL"
)

setMethod("initialize", "MLControl",
  function(.Object, summary = resampleSummary, cutoff = 0.5,
           cutoff.index = function(sens, spec) sens + spec,
           survtimes = numeric(), ...) {
    callNextMethod(.Object, summary = summary, cutoff = cutoff,
                   cutoff.index = cutoff.index, survtimes = survtimes, ...)
  }
)

setAs("MLControl", "list",
  function(from) {
    list(cutoff = from@cutoff,
         cutoff.index = from@cutoff.index,
         survtimes = from@survtimes)
  }
)


setClass("MLModel",
  slots = c(name = "character", params = "list", fit = "function",
            predict = "function")
)

MLModel <- function(...) new("MLModel", ...)


setClass("MLModelFit",
  slots = c(.predict = "function"),
  contains ="VIRTUAL"
)

asMLModelFit <- function(object, Class, model) {
  if(!inherits(model, "MLModel")) stop("model not of class MLModel")
  predict <- model@predict
  if(isS4(object)) {
    object <- as(object, Class)
    if(!inherits(object, "MLModelFit")) stop("Class not from MLModelFit")
    object@.predict <- predict
  } else if(is.list(object)) {
    class(object) <- c(Class, "MLModelFit", class(object))
    object$.predict <- predict
  } else {
    stop("unsupported object class")
  }
  object
}

asParentFit <- function(object) {
  if(!inherits(object, "MLModelFit")) stop("object not of class MLModelFit")
  if(isS4(object)) {
    as(object, extends(class(object))[3])
  } else {
    structure(object, class = class(object)[-(1:2)])
  }
}
