setClass("MLControl",
  slots = c(summary = "function", cutoff = "numeric", cutoff.index = "function",
            survtimes = "numeric"),
  contains = "VIRTUAL"
)

setMethod("initialize", "MLControl",
  function(.Object, summary = modelmetrics, cutoff = 0.5,
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


setClass("BootControl",
  slots = c(number = "numeric"),
  contains = "MLControl"
)

BootControl <- function(number = 100, ...) {
  new("BootControl", number = number, ...)
}


setClass("CVControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl"
)

CVControl <- function(folds = 10, repeats = 1, ...) {
  new("CVControl", folds = folds, repeats = repeats, ...)
}


setClass("MLModel",
  slots = c(name = "character",
            packages = "character",
            responses = "character",
            params = "list",
            fit = "function",
            predict = "function",
            response = "function",
            varimp = "function")
)

MLModel <- function(...) new("MLModel", ...)


setClass("MLModelFit",
  slots = c(.predict = "function",
            .response = "function",
            .varimp = "function"),
  contains ="VIRTUAL"
)

asMLModelFit <- function(object, Class, model) {
  if(!inherits(model, "MLModel")) stop("model not of class MLModel")
  predict <- model@predict
  response <- model@response
  varimp <- model@varimp
  if(isS4(object)) {
    object <- as(object, Class)
    if(!inherits(object, "MLModelFit")) stop("Class not from MLModelFit")
    object@.predict <- predict
    object@.response <- response
    object@.varimp <- varimp
  } else if(is.list(object)) {
    class(object) <- c(Class, "MLModelFit", class(object))
    object$.predict <- predict
    object$.response <- response
    object$.varimp <- varimp
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


setClass("SVMFit", contain = c("MLModelFit", "ksvm"))
setClass("CForestFit", contains = c("MLModelFit", "RandomForest"))


setClass("Resamples", contains = "data.frame")

setAs("matrix", "Resamples",
  function(from) new("Resamples", as.data.frame(from))
)


setClass("VarImp", contains = "data.frame")

setMethod("initialize", "VarImp",
  function(.Object, .Data, scale = FALSE, ...) {
    idx <- order(.Data[[1]], decreasing = TRUE)
    idx <- idx * (rownames(.Data)[idx] != "(Intercept)")
    .Data <- .Data[idx, , drop = FALSE]
    if(scale) .Data <- 100 * (.Data - min(.Data)) / diff(range(.Data))
    callNextMethod(.Object, .Data, ...)
  }
)

setAs("data.frame", "VarImp",
  function(from) new("VarImp", from)
)

setAs("matrix", "VarImp",
  function(from) as(as.data.frame(from), "VarImp")
)

setAs("vector", "VarImp",
  function(from) as(data.frame(Overall = from), "VarImp")
)

setValidity("VarImp", function(object) {
  !(nrow(object) && is.null(rownames(object)))
})
