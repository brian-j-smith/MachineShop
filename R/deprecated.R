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


#' @rdname deprecated
#' 
tune <- function(...) {
  depwarn("tune() is deprecated",
          "call fit() with a SelectedModel or TunedModel instead",
          expired = Sys.Date() >= "2020-01-01")
  (function(x, ...) tune_model(x, ...))(...)
}
