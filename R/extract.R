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
#' @aliases [.BinomialMatrix
#' 
"[.BinomialMatrix" <- function(x, i, j, ..., drop = FALSE) {
  y <- if (missing(j)) unclass(x)[i, , drop = drop] else NextMethod(drop = drop)
  if (identical(colnames(x), colnames(y))) {
    structure(y, class = "BinomialMatrix")
  } else y
}


#' @rdname extract-methods
#' @aliases [,DiscreteVector,ANY,missing,missing-method
#' 
setMethod("[",
  c(x = "DiscreteVector", i = "ANY", j = "missing", drop = "missing"),
  function(x, i) {
    new(class(x), asS3(x)[i], min = x@min, max = x@max)
  }
)


#' @rdname extract-methods
#' @aliases [,Resamples,ANY,ANY,ANY-method
#' 
setMethod("[", c(x = "RecipeGrid", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    x <- asS3(x)
    callNextMethod()
  }
)


#' @rdname extract-methods
#' @aliases [,Resamples,ANY,ANY,ANY-method
#' 
setMethod("[", c(x = "Resamples", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    y <- asS3(x)[i, j, drop = drop]
    if (identical(colnames(x), colnames(y))) {
      y$Model <- droplevels(y$Model)
      new("Resamples", y, control = x@control, strata = x@strata)
    } else y
  }
)


#' @rdname extract-methods
#' @aliases [,Resamples,ANY,missing,ANY-method
#' 
setMethod("[", c(x = "Resamples", i = "ANY", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    if (nargs() < 3) x[, i, drop = FALSE] else x[i, TRUE, drop = drop]
  }
)


#' @rdname extract-methods
#' @aliases [,Resamples,missing,missing,ANY-method
#' 
setMethod("[", c(x = "Resamples", i = "missing", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    x[, TRUE, drop = drop]
  }
)


#' @rdname extract-methods
#' @aliases [,SurvMatrix,ANY,ANY,ANY-method
#' 
setMethod("[", c(x = "SurvMatrix", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    y <- asS3(x)[i, j, drop = drop]
    if (is.matrix(y)) {
      new(class(x), y, times = x@times[match(colnames(y), colnames(x))])
    } else y
  }
)


#' @rdname extract-methods
#'
#' @param e1,e2 objects.
#'
setMethod("+", c("SurvMatrix", "SurvMatrix"),
  function(e1, e2) {
    x <- callNextMethod()
    class <- class(e1)
    if (class(e2) == class && all(e1@times == e2@times)) {
      new(class, x, times = e1@times)
    } else x
  }
)
