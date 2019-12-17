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
        stop(label_items("response factor has new level", new_levels))
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


.response_types <- c("binary", "BinomialVariate", "DiscreteVariate", "factor",
                     "matrix", "NegBinomialVariate", "numeric", "ordered",
                     "PoissonVariate", "Surv")


#################### Discrete Variate Classes ####################


#' Discrete Variate Constructors
#'
#' Create a variate of binomial counts, discrete numbers, negative binomial
#' counts, or Poisson counts.
#'
#' @name DiscreteVariate
#' @rdname DiscreteVariate
#'
#' @param x numeric vector.
#' @param size number or numeric vector of binomial trials.
#' @param min,max minimum and maximum bounds for discrete numbers.
#'
#' @return \code{BinomialVariate} object class that inherits from \code{matrix},
#' \code{DiscreteVariate} that inherits from \code{numeric}, or object of the
#' same class as the constructor name and that inherits from
#' \code{DiscreteVariate}.
#'
#' @seealso \code{\link{role_binom}}
#'
#' @examples
#' BinomialVariate(rbinom(25, 10, 0.5), size = 10)
#' PoissonVariate(rpois(25, 10))
#'
BinomialVariate <- function(x = integer(), size = integer()) {
  stopifnot(is.finite(size))
  stopifnot(length(size) <= 1 || length(size) == length(x))
  x <- as.integer(x)
  size <- as.integer(size)
  stopifnot(x >= 0)
  stopifnot(size >= x)
  structure(cbind(Success = x, Failure = size - x), class = "BinomialVariate")
}


#' @rdname DiscreteVariate
#'
DiscreteVariate <- function(x = integer(), min = -Inf, max = Inf) {
  x <- as.integer(x)
  min <- floor(min[[1]])
  max <- floor(max[[1]])
  stopifnot(min <= x)
  stopifnot(max >= x)
  new("DiscreteVariate", x, min = min, max = max)
}


#' @rdname DiscreteVariate
#'
NegBinomialVariate <- function(x = integer()) {
  as(DiscreteVariate(x, min = 0), "NegBinomialVariate")
}


#' @rdname DiscreteVariate
#'
PoissonVariate <- function(x = integer()) {
  as(DiscreteVariate(x, min = 0), "PoissonVariate")
}


#################### SurvMatrix Classes ####################


#' SurvMatrix Class Constructors
#'
#' Create a matrix of survival events or probabilites.
#'
#' @name SurvMatrix
#' @rdname SurvMatrix
#'
#' @param data matrix, or object that can be coerced to one, with survival
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


SurvMatrix <- function(data = NA, times = NULL) {
  data <- as.matrix(data)

  if (is.null(times)) times <- rep(NA_real_, ncol(data))

  if (length(times) != ncol(data)) {
    stop("unequal number of survival times and predictions")
  }

  rownames(data) <- NULL
  colnames(data) <- if (length(times)) paste("Time", seq_along(times))

  new("SurvMatrix", data, times = times)
}


#' @rdname SurvMatrix
#'
SurvEvents <- function(data = NA, times = NULL) {
  as(SurvMatrix(data, times), "SurvEvents")
}


#' @rdname SurvMatrix
#'
SurvProbs <- function(data = NA, times = NULL) {
  as(SurvMatrix(data, times), "SurvProbs")
}
