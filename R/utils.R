utils::globalVariables(c("Found", "group", "i", "Lower", "Mean", "Midpoint",
                         "model", "Model", "Predictor", "Response", "Tested",
                         "Upper", "Value", "values", "variables", "y", "..y.."))


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


basehaz <- function(y, risk, times) {
  y_times <- unique(y[, "time"]) %>% sort
  nrisk <- rowsum(risk, y[, "time"]) %>% rev %>% cumsum %>% rev
  nevent <- rowsum(y[, "status"], y[, "time"])[, 1]
  cumhaz <- cumsum(nevent / nrisk) %>% structure(names = NULL)
  idx <- approx(y_times, seq(y_times), times, method = "constant",
                f = 0, yleft = 0, yright = length(y_times))$y
  c(0, cumhaz)[idx + 1]
}


setGeneric("append", function(x, y, ...) standardGeneric("append"))


setMethod("append", c("data.frame", "data.frame"),
  function(x, y) {
    stopifnot(names(x) == names(y))
    df <- data.frame(matrix(nrow = nrow(x) + nrow(y), ncol = 0))
    for (varname in names(x)) {
      df[[varname]] <- append(x[[varname]], y[[varname]])
    }
    df
  }
)


setMethod("append", c("factor", "factor"),
  function(x, y) unlist(list(x, y))
)


setMethod("append", c("matrix", "matrix"),
  function(x, y) rbind(x, y)
)


setMethod("append", c("Surv", "Surv"),
  function(x, y) {
    df <- as.data.frame(rbind(x, y))
    names(df) <- NULL
    do.call(Surv, df)
  }
)


setMethod("append", c("vector", "vector"),
  function(x, y) c(x, y)
)


fitbit <- function(object, name) {
  fitbits <- if (isS4(object)) object@fitbits else object$fitbits
  slot(fitbits, name)
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
  if (is.character(x)) x <- get(x, mode = "function")
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
           fo <- formula(modelterms)
           mf <- model.frame(fo, data)
           ncol(model.matrix(fo, mf)) - attr(modelterms, "intercept")
         }
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
  if (is.null(data)) x else data
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
