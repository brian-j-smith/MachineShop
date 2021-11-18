#' Extract Elements of an Object
#'
#' Operators acting on data structures to extract elements.
#'
#' @name extract
#' @rdname extract-methods
#'
#' @param x object from which to extract elements.
#' @param i,j,... indices specifying elements to extract.
#' @param drop logical indicating that the result be returned as an object
#'   coerced to the lowest dimension possible if \code{TRUE} or
#'   with the original dimensions and class otherwise.
#'
NULL


#' @rdname extract-methods
#'
"[.BinomialVariate" <- function(x, i, j, ..., drop = FALSE) {
  y <- if (missing(j)) unclass(x)[i, , drop = drop] else NextMethod(drop = drop)
  if (identical(colnames(x), colnames(y))) {
    structure(y, class = "BinomialVariate")
  } else y
}


#' @rdname extract-methods
#'
setMethod("[",
  c(x = "DiscreteVariate", i = "ANY", j = "missing", drop = "missing"),
  function(x, i) {
    new(class(x), asS3(x)[i], min = x@min, max = x@max)
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "ListOf", i = "ANY", j = "missing", drop = "missing"),
  function(x, i) {
    ListOf(asS3(x)[i])
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "ModelFrame", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    y <- as.data.frame(x)[i, j, drop = drop]
    if (identical(colnames(x), colnames(y))) {
      y <- ModelFrame(terms(x), y, na.rm = FALSE)
      if (is(x, "ModeledInput")) {
        y <- new(class(x), y, model = x@model)
      } else if (is(x, "SelectedInput")) {
        y <- new(class(x), y, inputs = x@inputs, params = x@params)
      }
    }
    y
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "ModelFrame", i = "ANY", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    ninds <- nargs() - 1 - !missing(drop)
    if (ninds > 1) {
      if (!drop) {
        as(asS3(x)[i, , drop = FALSE], class(x))
      } else x[i, TRUE, drop = TRUE]
    } else x[, i, drop = FALSE]
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "ModelFrame", i = "missing", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    x[TRUE, j, drop = drop]
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "ModelFrame", i = "missing", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    x[TRUE, , drop = drop]
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "RecipeGrid", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    asS3(x)[i, j, drop = drop]
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "Resamples", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    y <- as.data.frame(x)[i, j, drop = drop]
    if (identical(colnames(x), colnames(y))) {
      Resamples(y, control = x@control, case_comps = x@case_comps)
    } else y
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "Resamples", i = "ANY", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    ninds <- nargs() - 1 - !missing(drop)
    if (ninds > 1) x[i, TRUE, drop = drop] else x[, i, drop = FALSE]
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "Resamples", i = "missing", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    x[, TRUE, drop = drop]
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "SurvMatrix", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    y <- asS3(x)[i, j, drop = drop]
    if (is.matrix(y)) {
      times <- x@times[match(colnames(y), colnames(x))]
      new(class(x), y, times = times, distr = x@distr)
    } else y
  }
)


#' @rdname extract-methods
#'
setMethod("[", c(x = "SurvTimes", i = "ANY", j = "missing", drop = "missing"),
  function(x, i) {
    new(class(x), asS3(x)[i], distr = x@distr)
  }
)
