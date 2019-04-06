#' Extract Response Variable
#' 
#' Extract the response variable from an object.
#' 
#' @rdname response-methods
#' 
#' @param object object containing response variable values.
#' @param data \code{data.frame} containing values at which to evaluate the
#' response variable.
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
#' response(fo, data = Melanoma)
#' 
response <- function(object, ...) {
  UseMethod("response")
}


response.MLFitBits <- function(object, data = NULL, ...) {
  if (is.null(data)) object@y else response(object@x, data)
}


#' @rdname response-methods
#' 
response.formula <- function(object, data = NULL, ...) {
  expr <- if (length(object) > 2) object[[2]]
  if (!is.null(data)) {
    vars <- all.vars(response(object))
    eval(expr, as.data.frame(data[, vars, drop = FALSE]))
  } else expr
}


#' @rdname response-methods
#' 
response.MLModelFit <- function(object, data = NULL, ...) {
  response(field(object, "fitbits"), data)
}


#' @rdname response-methods
#' 
response.ModelFrame <- function(object, data = NULL, ...) {
  response(terms(object), if (is.null(data)) object else data)
}


#' @rdname response-methods
#' 
response.recipe <- function(object, data = NULL, ...) {
  object <- prep(object)
  data <- if (is.null(data)) juice(object) else bake(object, data)
  response(terms(object), data)
}
