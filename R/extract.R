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
      new(class(x), y, times = x@times[colnames(x) %in% colnames(y)])
    } else y
  }
)
