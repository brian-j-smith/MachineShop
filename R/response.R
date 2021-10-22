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
#'
#' mf <- ModelFrame(Surv(time, status) ~ ., data = veteran)
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
        throw(Error(label_items("response factor has new level", new_levels)))
      }
      y <- factor(y, levels = template_levels, ordered = is.ordered(template),
                  exclude = NULL)
    }
    template_class <- class1(template)
    if (!(is.null(template) || is(y, template_class))) {
      throw(TypeError(y, template_class, "response variable"))
    }
    y
  } else if (length(object) > 2) object[[2]]
}


response.MLModel <- function(object, newdata = NULL, ...) {
  response(object@input, newdata)
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
  if (!is.null(newdata)) {
    f <- if (is(newdata, "ModelFrame")) formula else terms
    y <- response(f(object), newdata, y)
  }
  y
}


#' @rdname response-methods
#'
response.recipe <- function(object, newdata = NULL, ...) {
  object <- prep(object)
  response(terms(object), bake(object, newdata))
}


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
#' @return \code{BinomialVariate} object class, \code{DiscreteVariate} that
#' inherits from \code{numeric}, or \code{NegBinomialVariate} or
#' \code{PoissonVariate} that inherit from \code{DiscreteVariate}.
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
#' @param distr character string specifying the survival distribution from which
#'   the matrix values were derived.
#'
#' @return Object that is of the same class as the constructor name and inherits
#' from \code{SurvMatrix}.  Examples of these are predicted survival events and
#' probabilities returned by the \link{predict} function.
#'
#' @seealso \code{\link{performance}}, \code{\link{metrics}}
#'
NULL


SurvMatrix <- function(data = NA, times = NULL, distr = NULL) {
  data <- as.matrix(data)

  if (is.null(times)) times <- rep(NA_real_, ncol(data))
  if (is.null(distr)) distr <- NA_character_

  if (length(times) != ncol(data)) {
    throw(Error("unequal number of survival times and predictions"))
  }

  rownames(data) <- NULL
  colnames(data) <- if (length(times)) paste("Time", seq_along(times))

  new("SurvMatrix", data, times = times, distr = distr)
}


#' @rdname SurvMatrix
#'
SurvEvents <- function(data = NA, times = NULL, distr = NULL) {
  as(SurvMatrix(data, times, distr), "SurvEvents")
}


#' @rdname SurvMatrix
#'
SurvProbs <- function(data = NA, times = NULL, distr = NULL) {
  as(SurvMatrix(data, times, distr), "SurvProbs")
}


SurvMeans <- function(...) {
  new("SurvMeans", SurvTimes(...))
}


SurvTimes <- function(x = numeric(), distr = NULL, ...) {
  if (is.null(distr)) distr <- NA_character_
  new("SurvTimes", x, distr = distr)
}


SurvPrediction <- function(object, ...) {
  (if (is.matrix(object)) SurvProbs else SurvMeans)(object, ...)
}
