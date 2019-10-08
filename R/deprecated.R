#' Deprecated Functions
#' 
#' Functions that have been deprecated and will be removed in the future.
#' 
#' @rdname deprecated
#' 
#' @param ... arguments passed to non-deprecated equivalent.
#' 
expand.model <- function(...) {
  depwarn("expand.model() is deprecated", "use expand_model() instead",
          expired = Sys.Date() >= "2019-11-15")
  (function(x, ...) expand_model(x, ...))(...)
}
