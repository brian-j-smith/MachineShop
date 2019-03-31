#' Extract Parts of an Object
#' 
#' Operators acting on data structures to extract parts.
#' 
#' @name extract
#' @rdname extract-methods
#' @aliases [,Resamples,ANY,ANY,ANY-method
#' 
#' @param x object from which to extract elements.
#' @param i,j indices specifying elements to extract.
#' @param drop logical indicating that the result be returned as a
#' \code{numeric} coerced to the lowest dimension possible if \code{TRUE} or
#' retained as the original 2-dimensional object class otherwise.
#' 
#' @seealso \code{\link{resample}}
#' 
setMethod("[", c(x = "Resamples", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, drop = FALSE) {
    if (drop) {
      callNextMethod()
    } else {
      j <- TRUE
      new("Resamples", callNextMethod(), control = x@control, strata = x@strata)
    }
  }
)


#' @rdname extract-methods
#' @aliases [.SurvMatrix
#' 
#' @seealso \code{\link{SurvMatrix}}
#' 
"[.SurvMatrix" <- function(x, i, j, drop = FALSE) {
  y <- unclass(x)[i, j, drop = drop]
  if (drop) y else structure(y, class = class(x), times = time(x)[j])
}
