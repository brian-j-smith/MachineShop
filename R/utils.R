utils::globalVariables(c("group", "i", "Lower", "Mean", "Midpoint", "model",
                         "Model", "Observed", "Predicted", "Predictor",
                         "Resample", "Response", "Upper", "Value", "values",
                         "variables", "x", "y", "..y.."))


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
    if (is.null(x[[i]][[which]])) x[[i]][[which]] <- rep(which, nrow(x[[i]]))
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


seq_inner <- function(from, to, length) {
  x <- seq(from, to, length = length + 2)
  x[-c(1, length + 2)]
}


seq_range <- function(from, by, lim, length) {
  if (length > 0) {
    to <- min(from + by * (length - 1), lim[2])
    x <- seq(from, to, length = length)
    x[x >= lim[1]]
  } else {
    seq(from, length = length)
  }
}


seq_nvars <- function(x, model, length) {
  nvars <- nvars(x, model)
  length <- min(length, nvars)
  vals <- if (length > 1) {
    if (nvars < 500) {
      seq(2, nvars, length = length)
    } else {
      2^seq(1, log2(nvars), length = length)
    }
  } else {
    numeric()
  }
  round(vals)
}

    
set_param <- function(params, name, value) {
  if (name %in% names(params)) params[[name]] <- value
  params
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
  var_name <- info$variable[info$role == "case_strata"]
  if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) var_name else
      stop("multiple strata variables specified")
}


switch_class <- function(EXPR, ...) {
  blocks <- eval(substitute(alist(...)))
  isClass <- sapply(names(blocks), function(class) is(EXPR, class))
  eval.parent(blocks[[match(TRUE, isClass)]])
}


terms.recipe <- function(x, ...) {
  info <- summary(x)
  
  get_vars <- function(roles = NULL, types = NULL) {
    is_match <- by(info, info$variable, function(split) {
      all(roles %in% split$role) && all(types %in% split$type)
    })
    names(is_match)[is_match]
  }
  
  outcome_set <- get_vars("outcome")

  surv_time <- get_vars(c("surv_time", "outcome"))
  surv_event <- get_vars(c("surv_event", "outcome"))
  numeric_outcomes <- get_vars("outcome", "numeric")  
  
  if (length(surv_time) > 1 || length(surv_event) > 1) {
    stop("multiple instances of outcome role 'surv_time' or 'surv_event'")
  } else if (length(surv_time)) {
    outcome <- call("Surv", as.symbol(surv_time))
    if (length(surv_event)) outcome[[3]] <- as.symbol(surv_event)
    outcome_set <- setdiff(outcome_set, c(surv_time, surv_event))
  } else if (length(surv_event)) {
    stop("outcome role 'surv_event' specified without 'surv_time'")
  } else if (length(numeric_outcomes) > 1) {
    outcome <- as.call(c(.(cbind), lapply(numeric_outcomes, as.symbol)))
    outcome_set <- setdiff(outcome_set, numeric_outcomes)
  } else if (length(outcome_set) == 1) {
    outcome <- outcome_set
    outcome_set <- NULL
  }
  
  if (length(outcome_set)) {
    stop("recipe outcome must be a single variable, survival variables with ",
         "roles 'surv_time' and 'surv_event', or multiple numeric variables")
  }

  predictors <- info$variable[info$role == "predictor"]
  
  terms(reformulate(predictors, outcome))
}


warn <- function(...) {
  warning(..., call. = FALSE)
}


depwarn <- function(old, new, expired = FALSE) {
  ifelse(expired, stop, warning)(old, ";\n", new, call. = FALSE)
}
