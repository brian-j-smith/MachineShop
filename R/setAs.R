as.data.frame.ModelFrame <- function(x, ...) {
  attr(x, "terms") <- NULL
  structure(x, class = "data.frame")
}


as.data.frame.recipe <- function(x, ...) {
  as.data.frame(x$template)
}


setAs("data.frame", "VarImp",
  function(from) VarImp(from)
)


setAs("matrix", "VarImp",
  function(from) as(as.data.frame(from), "VarImp")
)


setAs("vector", "VarImp",
  function(from) as(data.frame(Overall = from), "VarImp")
)


asMLModelFit <- function(object, Class, model, x, y) {
  fitbits <- model@fitbits
  fitbits@x <- x
  fitbits@y <- y

  if (is(object, Class)) {
    object <- unMLModelFit(object)
  } else if (is(object, "MLModelFit")) {
    stop("cannot change MLModelFit class")
  }
  
  if (!is(model, "MLModel")) stop("model not of class MLModel")
  
  if (isS4(object)) {
    object <- new(Class, object, fitbits = fitbits)
  } else if (is.list(object)) {
    object$fitbits <- fitbits
    class(object) <- c(Class, "MLModelFit", class(object))
  } else {
    stop("unsupported object class")
  }

  object
}


unMLModelFit <- function(object) {
  if (!is(object, "MLModelFit")) return(object)
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
