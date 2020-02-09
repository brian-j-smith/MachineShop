#' Deprecated Functions
#'
#' Functions that have been deprecated and will be removed in the future.
#'
#' @name deprecated
#' @rdname deprecated
#'
#' @param ... arguments passed to non-deprecated equivalent.
#'
NULL


#' @rdname deprecated
#'
#' @details
#' Use \code{\link[=SelectedInput]{SelectedInput()}} instead of
#' \code{SelectedFormula()}.
#'
SelectedFormula <- function(...) {
  depwarn("SelectedFormula() is deprecated", "use SelectedInput() instead",
          expired = Sys.Date() >= "2020-03-01")
  SelectedInput(...)
}


#' @rdname deprecated
#'
#' @details
#' Use \code{\link[=SelectedInput]{SelectedInput()}} instead of
#' \code{SelectedMatrix()}.
#'
SelectedMatrix <- function(...) {
  depwarn("SelectedMatrix() is deprecated", "use SelectedInput() instead",
          expired = Sys.Date() >= "2020-03-01")
  SelectedInput(...)
}


#' @rdname deprecated
#'
#' @details
#' Use \code{\link[=SelectedInput]{SelectedInput()}} instead of
#' \code{SelectedModelFrame()}.
#'
SelectedModelFrame <- function(...) {
  depwarn("SelectedModelFrame() is deprecated", "use SelectedInput() instead",
          expired = Sys.Date() >= "2020-03-01")
  SelectedInput(...)
}


#' @rdname deprecated
#'
#' @details
#' Use \code{\link[=SelectedInput]{SelectedInput()}} instead of
#' \code{SelectedRecipe()}.
#'
SelectedRecipe <- function(...) {
  depwarn("SelectedRecipe() is deprecated", "use SelectedInput() instead",
          expired = Sys.Date() >= "2020-03-01")
  SelectedInput(...)
}


#' @rdname deprecated
#'
#' @details
#' Use \code{\link[=TunedInput]{TunedInput()}} instead of \code{TunedRecipe()}.
#'
TunedRecipe <- function(...) {
  depwarn("TunedRecipe() is deprecated", "use TunedInput() instead",
          expired = Sys.Date() >= "2020-03-01")
  TunedInput(...)
}
