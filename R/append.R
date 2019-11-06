append <- function(...) {
  Reduce(.append, list(...))
}


setGeneric(".append", function(x, y, ...) standardGeneric(".append"))


setMethod(".append", c("ANY", "missing"),
  function(x, y) x
)


setMethod(".append", c("data.frame", "data.frame"),
  function(x, y) {
    stopifnot(names(x) == names(y))
    df <- data.frame(matrix(nrow = nrow(x) + nrow(y), ncol = 0))
    for (varname in names(x)) {
      df[[varname]] <- .append(x[[varname]], y[[varname]])
    }
    df
  }
)


setMethod(".append", c("factor", "factor"),
  function(x, y) unlist(list(x, y))
)


setMethod(".append", c("matrix", "matrix"),
  function(x, y) rbind(x, y)
)


setMethod(".append", c("ordered", "ordered"),
  function(x, y) {
    xy <- unlist(list(x, y))
    if (all(levels(x) == levels(y))) as.ordered(xy) else xy
  }
)


setMethod(".append", c("Surv", "Surv"),
  function(x, y) c(x, y)
)


setMethod(".append", c("SurvMatrix", "SurvMatrix"),
  function(x, y) {
    stopifnot(identical(x@times, y@times))
    class <- class(x)
    if (class != class(y)) class <- "SurvMatrix"
    new(class, rbind(x, y), times = x@times)
  }
)


setMethod(".append", c("vector", "vector"),
  function(x, y) c(x, y)
)
