setOldClass("recipe")


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

BootControl <- function(number = 25, ...) {
  new("BootControl", number = number, ...)
}


setClass("CVControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl"
)

CVControl <- function(folds = 10, repeats = 1, ...) {
  new("CVControl", folds = folds, repeats = repeats, ...)
}


setClass("OOBControl",
  slots = c(number = "numeric"),
  contains = "MLControl"
)

OOBControl <- function(number = 25, ...) {
  new("OOBControl", number = number, ...)
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

asMLModelFit <- function(object, Class, model = NULL) {
  if(isS4(object)) {
    object <- as(object, Class)
    if(!inherits(object, "MLModelFit")) stop("Class not from MLModelFit")
  } else if(is.list(object)) {
    class(object) <- c(Class, "MLModelFit", class(object))
  } else {
    stop("unsupported object class")
  }
  if(!is.null(model)) {
    if(!inherits(model, "MLModel")) stop("model not of class MLModel")
    field(object, ".predict") <- model@predict
    field(object, ".response") <- model@response
    field(object, ".varimp") <- model@varimp
  }
  object
}

asParentFit <- function(object) {
  if(!inherits(object, "MLModelFit")) stop("object not of class MLModelFit")
  if(isS4(object)) {
    classes <- extends(class(object))
    from <- match("MLModelFit", classes) + 1
    as(object, classes[from])
  } else {
    object[c(".predict", ".response", ".varimp")] <- NULL
    classes <- class(object)
    from <- match("MLModelFit", classes) + 1
    to <- length(classes)
    structure(object, class = classes[from:to])
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
