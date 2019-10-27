#' Extract Response Variable
#' 
#' Extract the response variable from an object.
#' 
#' @rdname response-methods
#' 
#' @param object model \link{fit} result, \code{\link{ModelFrame}}, or
#'   \code{\link[recipes]{recipe}}.
#' @param newdata \link[=data.frame]{data frame} from which to extract the
#'   response variable values if given; otherwise, \code{object} is used.
#' @param ... arguments passed to other methods.
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
  if (is.null(newdata)) object@y else response(object@x, newdata)
}


response.formula <- function(object, data = NULL, template = NULL, ...) {
  if (!is.null(data)) {
    expr <- response(object)
    y <- eval(expr, as.data.frame(data[, all.vars(expr), drop = FALSE]))
    if (is.factor(template)) {
      y_levels <- as.character(unique(y))
      template_levels <- levels(template)
      new_levels <- y_levels[is.na(match(y_levels, template_levels))]
      if (length(new_levels)) {
        stop("response factor has new level", plural_suffix(new_levels), ": ",
             toString(new_levels))
      }
      y <- factor(y, levels = template_levels, ordered = is.ordered(template),
                  exclude = NULL)
    }
    template_class <- class(template)[1]
    if (!(is.null(template) || is(y, template_class))) {
      stop("response variable must be of type ", template_class)
    }
    y
  } else if (length(object) > 2) object[[2]]
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
  if (is.null(newdata)) y else response(terms(object), newdata, y)
}


#' @rdname response-methods
#' 
response.recipe <- function(object, newdata = NULL, ...) {
  object <- prep(object)
  data <- if (is.null(newdata)) juice(object) else bake(object, newdata)
  response(terms(object), data)
}
