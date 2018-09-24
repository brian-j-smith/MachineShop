setGeneric("convert", function(object, x, ...) standardGeneric("convert"))


setMethod("convert", c("ANY", "ANY"), function(object, x, ...) x)


setMethod("convert", c("factor", "factor"), function(object, x, ...) x)


setMethod("convert", c("factor", "matrix"),
  function(object, x, ...) {
    n <- nlevels(object)
    x <- if (n > 2) {
      factor(max.col(x), levels = 1:n, labels = levels(object))
    } else {
      x[, ncol(x)]
    }
    convert(object, x, ...)
  }
)


setMethod("convert", c("factor", "numeric"),
  function(object, x, cutoff = 0.5, ...) {
    factor(x > cutoff, levels = c(FALSE, TRUE), labels = levels(object))
  }
)


setMethod("convert", c("integer", "numeric"),
  function(object, x, cutoff = 0.5, ...) {
    pm <- sign(x)
    abs_x <- abs(x)
    pm * ifelse(abs_x %% 1 > cutoff, ceiling(abs_x), floor(abs_x))
  }
)


setMethod("convert", c("Surv", "matrix"),
  function(object, x, cutoff = 0.5, ...) {
    x <- x <= cutoff
    mode(x) <- "integer"
    x
  }
)
