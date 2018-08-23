setClass("AbstractControl",
  slots = c(summary = "function", cutoff = "numeric", cutoff.index = "function",
            survtimes = "numeric"),
  contains = "VIRTUAL"
)

setMethod("initialize", "AbstractControl",
  function(.Object, summary = validateSummary, cutoff = 0.5,
           cutoff.index = function(sens, spec) sens + spec,
           survtimes = numeric(), ...) {
    callNextMethod(.Object, summary = summary, cutoff = cutoff,
                   cutoff.index = cutoff.index, survtimes = survtimes, ...)
  }
)

setAs("AbstractControl", "list",
  function(from) {
    list(cutoff = from@cutoff,
         cutoff.index = from@cutoff.index,
         survtimes = from@survtimes)
  }
)


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
