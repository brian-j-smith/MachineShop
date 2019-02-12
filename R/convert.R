setGeneric("convert_prob",
           function(object, x, ...) standardGeneric("convert_prob"))


setMethod("convert_prob", c("ANY", "ANY"),
  function(object, x, ...) x
)


setMethod("convert_prob", c("factor", "array"),
  function(object, x, ...) {
    convert_prob(object, adrop(x, length(dim(x))))
  }
)


setMethod("convert_prob", c("factor", "matrix"),
  function(object, x, ...) {
    if (nlevels(object) == 2) x[, ncol(x)] else x
  }
)


setMethod("convert_prob", c("matrix", "array"),
  function(object, x, ...) {
    convert_prob(object, adrop(x, length(dim(x))))
  }
)


setMethod("convert_prob", c("matrix", "matrix"),
  function(object, x, ...) {
    stopifnot(ncol(object) == ncol(x))
    x
  }
)


setMethod("convert_prob", c("numeric", "array"),
  function(object, x, ...) {
    convert_prob(object, adrop(x, length(dim(x))))
  }
)


setMethod("convert_prob", c("numeric", "matrix"),
  function(object, x, ...) {
    stopifnot(ncol(x) == 1)
    drop(x)
  }
)


setMethod("convert_prob", c("Surv", "matrix"),
  function(object, x, times, ...) {
    SurvProbs(x, times)
  }
)


setGeneric("convert_response",
           function(object, x, ...) standardGeneric("convert_response"))


setMethod("convert_response", c("ANY", "ANY"),
  function(object, x, ...) x
)


setMethod("convert_response", c("factor", "factor"),
  function(object, x, ...) x
)


setMethod("convert_response", c("factor", "matrix"),
  function(object, x, ...) {
    factor(max.col(x), levels = 1:nlevels(object), labels = levels(object))
  }
)


setMethod("convert_response", c("factor", "numeric"),
  function(object, x, cutoff = 0.5, ...) {
    factor(x > cutoff, levels = c(FALSE, TRUE), labels = levels(object))
  }
)


setMethod("convert_response", c("integer", "numeric"),
  function(object, x, cutoff = 0.5, ...) {
    pm <- sign(x)
    abs_x <- abs(x)
    pm * ifelse(abs_x %% 1 > cutoff, ceiling(abs_x), floor(abs_x))
  }
)


setMethod("convert_response", c("ordered", "matrix"),
  function(object, x, ...) {
    ordered(max.col(x), levels = 1:nlevels(object), labels = levels(object))
  }
)


setMethod("convert_response", c("Surv", "SurvEvents"),
  function(object, x, ...) x
)


setMethod("convert_response", c("Surv", "SurvProbs"),
  function(object, x, cutoff = 0.5, ...) {
    events <- x <= cutoff
    mode(events) <- "integer"
    SurvEvents(events, x@times)
  }
)
