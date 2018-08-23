setClass("AbstractModel",
  slots = c(params = "list", fit = "function", predict = "function"),
  contains = "VIRTUAL"
)

setMethod("initialize", "AbstractModel",
  function(.Object, ...) {
    .Object@params <- list(...)
    .Object@fit <- function(formula, data, ...) NULL
    .Object@predict <- function(object, data, ...) NULL
    .Object
  }
)
