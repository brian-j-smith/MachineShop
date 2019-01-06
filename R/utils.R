utils::globalVariables(c("Found", "group", "i", "Lower", "Mean", "Midpoint",
                         "model", "Model", "Observed", "Predicted", "Predictor",
                         "Response", "Tested", "Upper", "Value", "values",
                         "variables", "y", "..y.."))


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
#' @seealso \code{\link{quote}}
#' 
#' @examples
#' ## Stepwise variable selection with BIC
#' library(MASS)
#' 
#' glmfit <- fit(medv ~ ., Boston, GLMStepAICModel(k = .(log(nobs))))
#' varimp(glmfit)
#' 
. <- function(expr) {
  eval(substitute(quote(expr)))
}


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
  function(x, y) {
    df <- as.data.frame(rbind(x, y))
    names(df) <- NULL
    do.call(Surv, df)
  }
)


setMethod(".append", c("vector", "vector"),
  function(x, y) c(x, y)
)


assert_equal_weights <- function(weights) {
  if (any(diff(weights) != 0)) {
    warn("model weights are not supported and will be ignored")
  }
}


attachment <- function(what, pos = 2L,
                       name = deparse(substitute(what), backtick = FALSE)) {
  make_attach <- attach
  make_attach(what, pos, name, warn.conflicts = FALSE)
  do.call(on.exit, list(substitute(detach(name))), envir = parent.frame())
}


extract <- function(formula, data, na.action = na.pass) {
  mf <- model.frame(formula, data, na.action = na.action)
  list(x = model.matrix(formula, mf)[, -1, drop = FALSE],
       y = model.response(mf))
}


field <- function(object, name) {
  if (isS4(object)) slot(object, name) else object[[name]]
}


fitbit <- function(object, name) {
  slot(field(object, "fitbits"), name)
}


formula.MLFitBits <- function(object) {
  formula(terms(object@x))
}


formula.MLModelFit <- function(object) {
  formula(field(object, "fitbits"))
}


getdata <- function(x, ...) {
  UseMethod("getdata")
}


getdata.data.frame <- function(x, ...) {
  x
}


getdata.recipe <- function(x, ...) {
  x$template
}


getMLObject <- function(x, class) {
  if (is.character(x)) x <- get(x)
  if (is.function(x)) x <- x()
  if (!is(x, class)) stop("object not of class ", class)
  x
}


is_response <- function(object, class2) {
  if (class2 == "binary") {
    is(object, "factor") && nlevels(object) == 2
  } else {
    is(object, class2)
  }
}


list2function <- function(x) {
  error_msg <- paste0("'", deparse(substitute(x)), "' must be a function, ",
                      "one or more function names, or a list of functions")
  if (is(x, "MLMetric")) x <- list(x)
  if (is(x, "vector")) {
    x <- as.list(x)
    metric_names <- character()
    for (i in seq(x)) {
      if (is(x[[i]], "character")) {
        metric_name <- x[[i]]
        x[[i]] <- get(metric_name, mode = "function")
      } else if (is(x[[i]], "MLMetric")) {
        metric_name <- x[[i]]@name
      } else if (is(x[[i]], "function")) {
        metric_name <- "metric"
      } else {
        stop(error_msg)
      }
      name <- names(x)[i]
      metric_names[i] <-
        if (is.null(name) || !nzchar(name)) metric_name else name
    }
    names(x) <- make.unique(metric_names)
    eval(bquote(function(...) unlist(lapply(.(x), function(x) x(...)))))
  } else if (is(x, "function")) {
    x
  } else {
    stop(error_msg)
  }
}


make_unique_levels <- function(x, which) {
  level_names <- list()
  for (i in seq(x)) {
    if (is.null(x[[i]][[which]])) x[[i]][[which]] <- which
    x[[i]][[which]] <- as.factor(x[[i]][[which]])
    arg_name <- names(x)[i]
    level_names[[i]] <- if (!is.null(arg_name) && nzchar(arg_name)) {
      rep(arg_name, nlevels(x[[i]][[which]]))
    } else {
      levels(x[[i]][[which]])
    }
  }
  level_names <- level_names %>% unlist %>% make.unique %>% relist(level_names)
  
  for (i in seq(x)) levels(x[[i]][[which]]) <- level_names[[i]]
  
  x
}


match_indices <- function(indices, choices) {
  lookup <- structure(seq_along(choices), names = choices)
  indices <- na.omit(names(lookup)[lookup[indices]])
  if (length(indices) == 0) {
    indices <- names(lookup)[1]
    warn("specified indices not found; using ", indices, " instead")
  }
  indices
}


nvars <- function(x, model) {
  model <- getMLObject(model, "MLModel")
  model_terms <- terms(x)
  switch(model@design,
         "model.matrix" = {
           fo <- formula(model_terms)
           mf <- model.frame(fo, x[1, , drop = FALSE])
           ncol(model.matrix(fo, mf)) - attr(model_terms, "intercept")
         },
         "terms" = length(labels(model_terms))
  )
}


params <- function(envir) {
  args <- as.list(envir)
  is_missing <- sapply(args, function(x) is.symbol(x) && !nzchar(x))
  if (any(is_missing)) stop("missing values for required argument(s) ",
                            toString(names(args)[is_missing]))
  args[!sapply(args, is.null)]
}


prep.data.frame <- function(x, ...) {
  x
}


preprocess <- function(x, data = NULL, ...) {
  UseMethod("preprocess")
}


preprocess.default <- function(x, data = NULL, ...) {
  as.data.frame(if (is.null(data)) x else data)
}


preprocess.recipe <- function(x, data = NULL, ...) {
  if (is.null(data)) juice(x) else bake(x, new_data = data)
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
  UseMethod("strata")
}


strata.default <- function(object, ...) {
  object
}


strata.matrix <- function(object, ...) {
  object[, 1]
}


strata.Surv <- function(object, ...) {
  object[, "status"]
}


strata_var <- function(object, ...) {
  UseMethod("strata_var")
}


strata_var.ModelFrame <- function(object, ...) {
  if ("(strata)" %in% names(object)) "(strata)" else NULL
}


strata_var.recipe <- function(object, ...) {
  info <- summary(object)
  strata_index <- which(info$role %in% "case_strata")
  if (length(strata_index) > 1) stop("multiple strata variables specified")
  if (length(strata_index) == 1) info$variable[strata_index] else NULL
}


switch_class <- function(EXPR, ...) {
  blocks <- eval(substitute(alist(...)))
  isClass <- sapply(names(blocks), function(class) is(EXPR, class))
  eval.parent(blocks[[match(TRUE, isClass)]])
}


terms.recipe <- function(x, ...) {
  info <- summary(x)
  lhs <- with(info, variable[role == "outcome"])
  rhs <- with(info, variable[role == "predictor"])
  terms(reformulate(rhs, lhs))
}


warn <- function(...) {
  warning(..., call. = FALSE)
}


depwarn <- function(old, new) {
  warn(old, "\n", new)
}
