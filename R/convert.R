setGeneric("convert_prob",
           function(object, x, ...) standardGeneric("convert_prob"))


setMethod("convert_prob", c("ANY", "ANY"),
  function(object, x, ...) x
)


setMethod("convert_prob", c("ANY", "numeric"),
  function(object, x, ...) unname(x)
)


setMethod("convert_prob", c("factor", "array"),
  function(object, x, ...) {
    convert_prob(object, adrop(x, length(dim(x))))
  }
)


setMethod("convert_prob", c("factor", "matrix"),
  function(object, x, ...) {
    if (nlevels(object) == 2) {
      convert_prob(object, x[, ncol(x)])
    } else {
      structure(x, dimnames = list(NULL, levels(object)))
    }
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
    varnames <- colnames(x)
    if (is.null(varnames)) varnames <- colnames(object)
    if (is.null(varnames)) varnames <- paste0("y", seq(ncol(x)))
    structure(x, dimnames = list(NULL, varnames))
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
    convert_prob(object, x[, 1])
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
  function(object, x, cutoff, ...) {
    factor(x > cutoff, levels = c(FALSE, TRUE), labels = levels(object))
  }
)


setMethod("convert_response", c("integer", "numeric"),
  function(object, x, ...) {
    round(x)
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
  function(object, x, cutoff, ...) {
    events <- x <= cutoff
    mode(events) <- "integer"
    SurvEvents(events, x@times)
  }
)
