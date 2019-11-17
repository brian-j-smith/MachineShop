#' Deprecated Functions
#' 
#' Functions that have been deprecated and will be removed in the future.
#' 
#' @rdname deprecated
#' 
#' @param ... arguments passed to non-deprecated equivalent.
#' 
tune <- function(...) {
  depwarn("tune() is deprecated",
          "call fit() with a SelectedModel or TunedModel instead",
          expired = TRUE)
}
