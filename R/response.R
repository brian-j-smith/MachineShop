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


response.formula <- function(object, data = NULL, template = NULL, ...) {
  if (!is.null(data)) {
    expr <- response(object)
    y <- eval(expr, as.data.frame(data[, all.vars(expr), drop = FALSE]))
    if (is.factor(template)) {
      y_levels <- as.character(unique(y))
      template_levels <- levels(template)
      new_levels <- y_levels[is.na(match(y_levels, template_levels))]
      if (length(new_levels)) {
        stop(plural_suffix("response factor has new level", new_levels), ": ",
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


response.MLModel <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) object@y else response(object@x, newdata)
}


#' @rdname response-methods
#' 
response.MLModelFit <- function(object, newdata = NULL, ...) {
  response(as.MLModel(object), newdata)
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


.response_types <- c("binary", "BinomialMatrix", "DiscreteVector", "factor",
                     "matrix", "NegBinomialVector", "numeric", "ordered",
                     "PoissonVector", "Surv")


#################### Discrete Variable Classes ####################


#' Discrete Variable Constructors
#' 
#' Create an integer matrix of binomial counts or vector of discrete numbers,
#' negative binomial counts, or Poisson counts.
#' 
#' @name DiscreteVector
#' @rdname DiscreteVector
#' 
#' @param count,size numeric vectors of binomial counts and trials.
#' @param x numeric vector.
#' @param min,max minimum and maximum bounds for discrete numbers.
#' 
#' @return \code{BinomialMatrix} object class that inherits from \code{matrix},
#' \code{DiscreteVector} that inherits from \code{numeric}, or object of the
#' same class as the constructor name and that inherits from
#' \code{DiscreteVector}.
#' 
#' @examples
#' BinomialMatrix(rbinom(25, 10, 0.5), size = 10)
#' PoissonVector(rpois(25, 10))
#' 
BinomialMatrix <- function(count = integer(), size = integer()) {
  stopifnot(is.finite(size))
  stopifnot(length(size) <= 1 || length(size) == length(count))
  count <- as.integer(count)
  size <- as.integer(size)
  stopifnot(count >= 0)
  stopifnot(size >= count)
  structure(cbind(Success = count, Failure = size - count),
            class = "BinomialMatrix")
}


as.data.frame.BinomialMatrix <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
}


as.double.BinomialMatrix <- function(x, ...) {
  as.numeric(x[, "Success"] / (x[, "Success"] + x[, "Failure"]))
}


#' @rdname DiscreteVector
#' 
DiscreteVector <- function(x = integer(), min = -Inf, max = Inf) {
  x <- as.integer(x)
  min <- floor(min[[1]])
  max <- floor(max[[1]])
  stopifnot(min <= x)
  stopifnot(max >= x)
  new("DiscreteVector", x, min = min, max = max)
}


#' @rdname DiscreteVector
#' 
NegBinomialVector <- function(x = integer()) {
  as(DiscreteVector(x, min = 0), "NegBinomialVector")
}


#' @rdname DiscreteVector
#' 
PoissonVector <- function(x = integer()) {
  as(DiscreteVector(x, min = 0), "PoissonVector")
}


#################### SurvMatrix Classes ####################


#' SurvMatrix Class Constructors
#' 
#' Create a matrix of survival events or probabilites.
#' 
#' @name SurvMatrix
#' @rdname SurvMatrix
#' 
#' @param object matrix, or object that can be coerced to one, with survival
#'   events or probabilities at points in time in the columns and cases in the
#'   rows.
#' @param times numeric vector of survival times for the columns.
#' 
#' @return Object that is of the same class as the constructor name and inherits
#' from \code{SurvMatrix}.  Examples of these are predicted survival events and
#' probabilities returned by the \link{predict} function.
#' 
#' @seealso \code{\link{performance}}, \code{\link{metrics}}
#' 
NULL


SurvMatrix <- function(object, times = NULL) {
  object <- as.matrix(object)
  
  if (is.null(times)) times <- rep(NA_real_, ncol(object))
  
  if (length(times) != ncol(object)) {
    stop("unequal number of survival times and predictions")
  }
  
  rownames(object) <- NULL
  colnames(object) <- if (length(times)) paste("Time", seq_along(times))
  
  new("SurvMatrix", object, times = times)
}


as.data.frame.SurvMatrix <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
}


#' @rdname SurvMatrix
#' 
SurvEvents <- function(object = numeric(), times = NULL) {
  as(SurvMatrix(object, times), "SurvEvents")
}


#' @rdname SurvMatrix
#' 
SurvProbs <- function(object = numeric(), times = NULL) {
  as(SurvMatrix(object, times), "SurvProbs")
}
