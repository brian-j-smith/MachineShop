response <- function(object, ...) {
  UseMethod("response", object)
}


response.data.frame <- function(object, ...) {
  model.response(object)
}


response.formula <- function(object, data, ...) {
  eval(object[[2]], data)
}


response.terms <- function(object, ...) {
  i <- attr(object, "response")
  all.vars(object)[i]
}


response.MLModelFit <- function(object, ...) {
  .response(object, ...)
}


.response <- function(object, ...) {
  UseMethod(".response", object)
}


.response.MLModelFit <- function(object, ...) {
  response <- field(object, ".response")
  response(object, ...)
}
