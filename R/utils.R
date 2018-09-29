utils::globalVariables(c("group", "i", "model", "values", "variables", "y"))


.onLoad <- function(libname, pkgname) {
  registerDoSEQ()
  invisible()
}


#' Quote Operator
#' 
#' Shorthand notation for the \code{\link{quote}} function.  The quote operator
#' simply returns its argument unevaluated and can be applied to any \R
#' expression.  Useful for calling model constructors with quoted parameter
#' values that are defined in terms of a model \code{formula}, \code{data},
#' \code{weights}, \code{nobs}, \code{nvars}, or \code{y}.
#' 
#' @param expr any syntactically valid \R expression.
#' 
#' @return 
#' The quoted (unevaluated) expression.
#' 
#' @seealso
#' \code{\link{quote}}
#' 
. <- function(expr) {
  eval(substitute(quote(expr)))
}


basehaz <- function(y, risk, times) {
  y_times <- unique(y[, "time"]) %>% sort
  nrisk <- rowsum(risk, y[, "time"]) %>% rev %>% cumsum %>% rev
  nevent <- rowsum(y[, "status"], y[, "time"])[, 1]
  cumhaz <- cumsum(nevent / nrisk) %>% structure(names = NULL)
  idx <- approx(y_times, seq(y_times), times, method = "constant",
                f = 0, yleft = 0, yright = length(y_times))$y
  c(0, cumhaz)[idx + 1]
}


field <- function(object, name) {
  if (isS4(object)) slot(object, name) else object[[name]]
}


"field<-" <- function(object, name, value) {
  if (isS4(object)) slot(object, name) <- value else object[[name]] <- value
  object
}


getMLObject <- function(x, class) {
  if (is.character(x)) x <- get(x, mode = "function")
  if (is.function(x)) x <- x()
  if (!is(x, class)) stop("object not of class ", class)
  x
}


match_indices <- function(indices, choices) {
  lookup <- structure(seq(choices), names = choices)
  indices <- na.omit(names(lookup)[lookup[indices]])
  if (length(indices) == 0) {
    indices <- names(lookup)[1]
    warning("specified indices not found; using ", indices, " instead")
  }
  indices
}


nvars <- function(data, design = c("terms", "model.matrix")) {
  modelterms <- terms(data)
  switch(match.arg(design),
         "terms" = length(labels(modelterms)),
         "model.matrix" = {
           ncol(model.matrix(modelterms, data)) - attr(modelterms, "intercept")
         }
  )
}


params <- function(envir) {
  args <- as.list(envir)
  is_missing <- sapply(args, function(x) is.symbol(x) && nchar(x) == 0)
  if (any(is_missing)) stop("missing values for required argument(s) ",
                            toString(names(args)[is_missing]))
  args[!sapply(args, is.null)]
}


requireModelNamespaces <- function(packages) {
  pass <- sapply(packages, requireNamespace)
  if (!all(pass)) stop("install required packages: ", toString(packages[!pass]))
  invisible(pass)
}


stepAIC_args <- function(formula, direction, scope) {
  if (is.null(scope$lower)) scope$lower <- ~ 1
  if (is.null(scope$upper)) scope$upper <- formula[-2]
  formula[-2] <- if (direction == "backward") scope$upper else scope$lower
  list(formula = formula, scope = scope)
}


strata <- function(object, ...) {
  UseMethod("strata", object)
}


strata.default <- function(object, ...) {
  object
}


strata.Surv <- function(object, ...) {
  object[, "status"]
}


switch_class <- function(EXPR, ...) {
  blocks <- eval(substitute(alist(...)))
  isClass <- sapply(names(blocks), function(class) is(EXPR, class))
  eval.parent(blocks[[match(TRUE, isClass)]])
}
