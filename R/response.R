#' Extract Response Variable
#' 
#' Extract the response variable from an object.
#' 
#' @rdname response-methods
#' 
#' @param object object containing a response.
#' @param ... arguments passed to other methods.
#' 
#' @examples
#' ## Survival response example
#' library(survival)
#' 
#' fo <- Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno +
#'                            meal.cal + wt.loss
#' response(fo, lung)
#' 
response <- function(object, ...) {
  UseMethod("response", object)
}


response.data.frame <- function(object, ...) {
  model.response(object)
}


#' @rdname response-methods
#' 
#' @param data data frame containing the values of a response variable defined in a formula.
#' 
response.formula <- function(object, data, ...) {
  eval(object[[2]], data)
}


response.terms <- function(object, ...) {
  i <- attr(object, "response")
  all.vars(object)[i]
}


response.MLModelFit <- function(object, ...) {
  response <- field(object, ".response")
  response(object)
}
