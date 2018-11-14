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
         times = from@surv_times)
  }
)


asMLModelFit <- function(object, Class, model) {
  if (is(object, Class)) {
    object <- unMLModelFit(object)
  } else if (is(object, "MLModelFit")) {
    stop("cannot change MLModelFit class")
  }
  
  if (!is(model, "MLModel")) stop("model not of class MLModel")
  
  if (isS4(object)) {
    object <- new(Class, object, fitbits = model@fitbits)
  } else if (is.list(object)) {
    object$fitbits <- model@fitbits
    class(object) <- c(Class, "MLModelFit", class(object))
  } else {
    stop("unsupported object class")
  }

  object
}


unMLModelFit <- function(object) {
  if (!is(object, "MLModelFit")) stop("object not of class MLModelFit")
  if (isS4(object)) {
    classes <- extends(class(object))
    as(object, classes[match("MLModelFit", classes) + 1])
  } else {
    object$fitbits <- NULL
    classes <- class(object)
    pos <- match("MLModelFit", classes)
    structure(object, class = classes[-c(pos - 1, pos)])
  }
}


unAsIs <- function(object) {
  classes <- class(object)
  if ("AsIs" %in% classes) class(object) <- classes[-match("AsIs", classes)]
  object
}
