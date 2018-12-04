setGeneric("convert_dim", function(object, x) standardGeneric("convert_dim"))


setMethod("convert_dim", c("ANY", "ANY"),
  function(object, x) x
)


setMethod("convert_dim", c("factor", "array"),
  function(object, x) {
    convert_dim(object, adrop(x, length(dim(x))))
  }
)


setMethod("convert_dim", c("factor", "matrix"),
  function(object, x) {
    if (nlevels(object) == 2) x[, ncol(x)] else x
  }
)


setMethod("convert_dim", c("matrix", "array"),
  function(object, x) {
    convert_dim(object, adrop(x, length(dim(x))))
  }
)


setMethod("convert_dim", c("matrix", "matrix"),
  function(object, x) {
    stopifnot(ncol(object) == ncol(x))
    x
  }
)


setMethod("convert_dim", c("numeric", "array"),
  function(object, x) {
    convert_dim(object, adrop(x, length(dim(x))))
  }
)


setMethod("convert_dim", c("numeric", "matrix"),
  function(object, x) {
    stopifnot(ncol(x) == 1)
    drop(x)
  }
)


setGeneric("convert_response",
           function(object, x, ...) standardGeneric("convert_response"))


setMethod("convert_response", c("ANY", "ANY"), function(object, x, ...) x)


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


setMethod("convert_response", c("Surv", "matrix"),
  function(object, x, cutoff = 0.5, ...) {
    x <- x <= cutoff
    mode(x) <- "integer"
    x
  }
)
