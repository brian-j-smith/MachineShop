response <- function(object, ...) {
  UseMethod("response", object)
}


response.data.frame <- function(object, ...) {
  model.response(object)
}


response.formula <- function(object, data, ...) {
  eval(object[[2]], data)
}


response.MLModelFit <- function(object, ...) {
  response <- if(isS4(object)) object@.response else object$.response
  response(object, ...)
}
