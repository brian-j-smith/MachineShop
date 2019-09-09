#' Extract Parts of an Object
#' 
#' Operators acting on data structures to extract parts.
#' 
#' @name extract
#' @rdname extract-methods
#' @aliases [,Resamples,ANY,ANY,ANY-method
#' 
#' @param x \code{\link{SurvMatrix}} object or \link{resample} result from which
#' to extract elements.
#' @param i,j indices specifying elements to extract.
#' @param drop logical indicating that the result be returned as a
#' \code{numeric} coerced to the lowest dimension possible if \code{TRUE} or
#' retained as the original 2-dimensional object class otherwise.
#' 
setMethod("[", c(x = "Resamples", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, drop = FALSE) {
    y <- callNextMethod()
    if (identical(colnames(x), colnames(y)) && !drop) {
      y$Model <- droplevels(y$Model)
      new("Resamples", y, control = x@control, strata = x@strata)
    } else y
  }
)


#' @rdname extract-methods
#' @aliases [.SurvMatrix
#' 
"[.SurvMatrix" <- function(x, i, j, drop = FALSE) {
  y <- unclass(x)[i, j, drop = drop]
  if (drop) y else structure(y, class = class(x), times = time(x)[j])
}
