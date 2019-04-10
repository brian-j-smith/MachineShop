#' Extract Response Variable
#' 
#' Extract the response variable from an object.
#' 
#' @rdname response-methods
#' 
#' @param object object containing the response variable definition.
#' @param newdata data frame from which to extract the response
#' variable values if given; otherwise, \code{object} is used.
#' @param ... arguments passed to other methods.
#' 
#' @seealso \code{\link[recipes]{recipe}}
#' 
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' fo <- Surv(time, status != 2) ~ sex + age + year + thickness + ulcer
#' response(fo, Melanoma)
#' 
response <- function(object, ...) {
  UseMethod("response")
}


response.MLFitBits <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) object@y else response(object@x, newdata)
}


#' @rdname response-methods
#' 
response.formula <- function(object, newdata = NULL, ...) {
  args <- list(...)
  if (!is.null(args$data)) newdata <- args$data
  
  expr <- if (length(object) > 2) object[[2]]
  if (!is.null(newdata)) {
    vars <- all.vars(response(object))
    eval(expr, as.data.frame(newdata[, vars, drop = FALSE]))
  } else expr
}


#' @rdname response-methods
#' 
response.MLModelFit <- function(object, newdata = NULL, ...) {
  response(field(object, "fitbits"), newdata)
}


#' @rdname response-methods
#' 
response.ModelFrame <- function(object, newdata = NULL, ...) {
  response(terms(object), if (is.null(newdata)) object else newdata)
}


#' @rdname response-methods
#' 
response.recipe <- function(object, newdata = NULL, ...) {
  object <- prep(object)
  newdata <- if (is.null(newdata)) juice(object) else bake(object, newdata)
  response(terms(object), newdata)
}
