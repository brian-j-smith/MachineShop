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
#' mf <- ModelFrame(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'                  data = Melanoma)
#' response(mf)
#' 
response <- function(object, ...) {
  UseMethod("response")
}


response.MLFitBits <- function(object, newdata = NULL, ...) {
  y <- object@y
  if (is.null(newdata)) y else response(object@x, newdata, levels(y))
}


response.formula <- function(object, data = NULL, levels = NULL, ...) {
  expr <- if (length(object) > 2) object[[2]]
  if (!is.null(data)) {
    vars <- all.vars(response(object))
    y <- eval(expr, as.data.frame(data[, vars, drop = FALSE]))
    if (is.factor(y) && !is.null(levels)) {
      y_levels <- levels(y)
      new_levels <- y_levels[is.na(match(y_levels, levels))]
      if (length(new_levels)) {
        stop("response factor has new levels ", toString(new_levels))
      }
      y <- factor(y, levels = levels, exclude = NULL)
    }
    y
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
  y <- model.response(object)
  if (is.null(newdata)) y else response(terms(object), newdata, levels(y))
}


#' @rdname response-methods
#' 
response.recipe <- function(object, newdata = NULL, ...) {
  object <- prep(object)
  newdata <- if (is.null(newdata)) juice(object) else bake(object, newdata)
  response(terms(object), newdata)
}
