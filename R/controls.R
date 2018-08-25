setClass("CVControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl")

CVControl <- function(...) new("CVControl", ...)

setMethod("initialize", "CVControl",
  function(.Object, folds = 10, repeats = 1, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@folds <- folds
    .Object@repeats <- repeats
    .Object
  }
)
