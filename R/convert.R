convert_numeric <- function(x, ...) {
  check_numeric(x, ..., nonempty = FALSE, na.fail = FALSE)
}


setGeneric("convert_predicted",
  function(object, x, ...) standardGeneric("convert_predicted")
)


setMethod("convert_predicted", c("ANY", "ANY"),
  function(object, x, ...) x
)


setMethod("convert_predicted", c("ANY", "numeric"),
  function(object, x, ...) unname(x)
)


setMethod("convert_predicted", c("BinomialVariate", "ANY"),
  function(object, x, ...) unname(drop(x))
)


setMethod("convert_predicted", c("factor", "array"),
  function(object, x, ...) {
    convert_predicted(object, adrop(x, ndim(x)))
  }
)


setMethod("convert_predicted", c("factor", "matrix"),
  function(object, x, ...) {
    stopifnot(ncol(x) > 0)
    if (nlevels(object) == 2) {
      convert_predicted(object, x[, ncol(x)])
    } else {
      structure(x, dimnames = list(NULL, levels(object)))
    }
  }
)


setMethod("convert_predicted", c("matrix", "array"),
  function(object, x, ...) {
    convert_predicted(object, adrop(x, ndim(x)))
  }
)


setMethod("convert_predicted", c("matrix", "matrix"),
  function(object, x, ...) {
    stopifnot(ncol(object) == ncol(x))
    var_names <- colnames(x)
    if (is.null(var_names)) var_names <- colnames(object)
    if (is.null(var_names)) var_names <- make_names_len(ncol(x), "y")
    structure(x, dimnames = list(NULL, var_names))
  }
)


setMethod("convert_predicted", c("numeric", "array"),
  function(object, x, ...) {
    n <- ndim(x)
    x <- if (n == 1) c(x) else adrop(x, n)
    convert_predicted(object, x)
  }
)


setMethod("convert_predicted", c("numeric", "matrix"),
  function(object, x, ...) {
    stopifnot(ncol(x) == 1)
    convert_predicted(object, x[, 1])
  }
)


setGeneric("convert_response",
  function(object, x, ...) standardGeneric("convert_response")
)


setMethod("convert_response", c("ANY", "ANY"),
  function(object, x, ...) x
)


setMethod("convert_response", c("DiscreteVariate", "numeric"),
  function(object, x, ...) {
    x <- round(x)
    if (object@min > -Inf) x <- pmax(x, object@min)
    if (object@max < Inf) x <- pmin(x, object@max)
    new(class(object), x, min = object@min, max = object@max)
  }
)


setMethod("convert_response", c("factor", "factor"),
  function(object, x, ...) x
)


setMethod("convert_response", c("factor", "matrix"),
  function(object, x, ...) {
    factor(max.col(x), levels = seq_len(nlevels(object)),
           labels = levels(object))
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
    ordered(max.col(x), levels = seq_len(nlevels(object)),
            labels = levels(object))
  }
)


setMethod("convert_response", c("Surv", "SurvProbs"),
  function(object, x, cutoff, ...) {
    events <- x < cutoff
    storage.mode(events) <- "integer"
    SurvEvents(events, x@times, distr = x@distr)
  }
)
