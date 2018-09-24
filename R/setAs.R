setAs("data.frame", "VarImp",
  function(from) VarImp(from)
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
         cutoff_index = from@cutoff_index,
         times = from@surv_times,
         na.rm = from@na.rm)
  }
)


asMLModelFit <- function(object, Class, model = NULL) {
  if(isS4(object)) {
    object <- as(object, Class)
    if(!is(object, "MLModelFit")) stop("Class not from MLModelFit")
  } else if(is.list(object)) {
    class(object) <- c(Class, "MLModelFit", class(object))
  } else {
    stop("unsupported object class")
  }
  if(!is.null(model)) {
    if(!is(model, "MLModel")) stop("model not of class MLModel")
    field(object, ".packages") <- model@packages
    field(object, ".predict") <- model@predict
    field(object, ".response") <- model@response
    field(object, ".varimp") <- model@varimp
  }
  object
}


unMLModelFit <- function(object) {
  if(!is(object, "MLModelFit")) stop("object not of class MLModelFit")
  if(isS4(object)) {
    classes <- extends(class(object))
    as(object, classes[match("MLModelFit", classes) + 1])
  } else {
    object[c(".packages", ".predict", ".response", ".varimp")] <- NULL
    classes <- class(object)
    structure(object, class = tail(classes, -match("MLModelFit", classes)))
  }
}


unAsIs <- function(object) {
  classes <- class(object)
  if("AsIs" %in% classes) class(object) <- classes[-match("AsIs", classes)]
  object
}
