setAs("data.frame", "VarImp",
  function(from) new("VarImp", from)
)


setAs("matrix", "VarImp",
  function(from) as(as.data.frame(from), "VarImp")
)


setAs("vector", "VarImp",
  function(from) as(data.frame(Overall = from), "VarImp")
)


setAs("MLControl", "list",
  function(from) {
    list(cutoff = from@cutoff,
         index.cutoff = from@index.cutoff,
         survtimes = from@survtimes)
  }
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
    field(object, ".packages") <- model@packages
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
    object[c(".packages", ".predict", ".response", ".varimp")] <- NULL
    classes <- class(object)
    from <- match("MLModelFit", classes) + 1
    to <- length(classes)
    structure(object, class = classes[from:to])
  }
}
