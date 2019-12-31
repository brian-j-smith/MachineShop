utils::globalVariables(c("i", "x", "y"))


.onLoad <- function(libname, pkgname) {
  registerDoSEQ()
  invisible()
}


#' Quote Operator
#'
#' Shorthand notation for the \code{\link{quote}} function.  The quote operator
#' simply returns its argument unevaluated and can be applied to any \R
#' expression.  Useful for calling model constructors with quoted parameter
#' values that are defined in terms of \code{nobs}, \code{nvars}, or \code{y}.
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
#' glm_fit <- fit(sale_amount ~ ., ICHomes, GLMStepAICModel(k = .(log(nobs))))
#' varimp(glm_fit)
#'
. <- function(expr) {
  eval(substitute(quote(expr)))
}


assert_equal_weights <- function(weights) {
  if (any(diff(weights) != 0)) {
    warn("model weights are not supported and will be ignored")
  }
}


attach_objects <- function(what, pos = 2L,
                           name = deparse(substitute(what), backtick = FALSE)) {
  make_attach <- attach
  make_attach(what, pos, name, warn.conflicts = FALSE)
  do.call(on.exit, list(substitute(detach(name))), envir = parent.frame())
}


combine_dataframes <- function(x, y = NULL) {
  if (is.null(y)) return(x)
  common_cols <- intersect(names(x), names(y))
  if (!identical(x[common_cols], y[common_cols])) {
    stop("common columns in data frames differ")
  }
  diff_cols <- setdiff(names(y), common_cols)
  x[diff_cols] <- y[diff_cols]
  x
}


complete_subset <- function(...) {
  is_complete <- complete.cases(...)
  map(function(x) subset(x, is_complete), list(...))
}


fget <- function(x) {
  f <- get0(x, mode = "function")
  if (is.null(f)) {
    msg <- if (is.character(x)) {
      paste0("function '", x, "' not found")
    } else {
      "invalid function"
    }
    stop(msg)
  }
  f
}


findS3Method <- function(generic, object) {
  generic_name <- as.character(substitute(generic))[1]
  classes <- substring(methods(generic_name), nchar(generic_name) + 2)
  class <- match_class(object, classes)
  if (is.na(class)) {
    stop(generic_name, " method not found for '", class(object)[1], "' class")
  }
  paste0(generic_name, ".", class)
}


get0 <- function(x, mode = "any") {
  if (is.character(x)) {
    x_expr <- str2lang(x)
    x_name <- x
    if (is.symbol(x_expr)) {
      base::get0(x_name, mode = mode)
    } else if (is.call(x_expr) && x_expr[[1]] == "::") {
      base::get0(as.character(x_expr[[3]]),
                 envir = asNamespace(x_expr[[2]]),
                 mode = mode)
    }
  } else if (mode %in% c("any", mode(x))) {
    x
  }
}


getMLObject <- function(x, class = c("MLControl", "MLMetric", "MLModel")) {
  class <- match.arg(class)
  x <- get0(x)
  if (is.function(x) && class %in% c("MLControl", "MLModel")) x <- x()
  if (!is(x, class)) stop("object not of class ", class)
  x
}


identical_elements <- function(x, transform = identity, ...) {
  target <- transform(x[[1]])
  compare <- function(current) identical(transform(current), target, ...)
  all(map_logi(compare, x[-1]))
}


is.trained <- function(x, ...) {
  UseMethod("is.trained")
}


is.trained.MLModel <- function(x, ...) {
  length(x@trainbits) > 0
}


is_one_element <- function(x, class) {
  length(x) == 1 && is(x[[1]], class)
}


is_response <- function(object, class2) {
  if (class2 == "binary") {
    is(object, "factor") && nlevels(object) == 2
  } else {
    is(object, class2)
  }
}


label_items <- function(label, x) {
  paste0(label, if (length(x) > 1) "s", ": ", toString(x))
}


list2function <- function(x) {
  error_msg <- paste0("'", deparse(substitute(x)), "' must be a function, ",
                      "function name, or vector of these")
  if (is(x, "MLMetric")) x <- list(x)
  if (is(x, "vector")) {
    x <- as.list(x)
    metric_names <- character()
    for (i in seq(x)) {
      if (is(x[[i]], "character")) {
        metric_name <- x[[i]]
        x[[i]] <- fget(metric_name)
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
    eval(bquote(function(...) unlist(map(function(x) x(...), .(x)))))
  } else if (is(x, "function")) {
    x
  } else {
    stop(error_msg)
  }
}


make_list_names <- function(x, prefix) {
  old_names <- names(x)
  names(x) <- if (length(x)) {
    if (length(x) > 1) paste0(prefix, ".", seq(x)) else prefix
  }
  if (!is.null(old_names)) {
    keep <- nzchar(old_names)
    names(x)[keep] <- old_names[keep]
  }
  names(x)
}


map <- function(f, ...) {
  all_args <- all(lengths(list(...)))
  if (all_args) mapply(FUN = f, ..., SIMPLIFY = FALSE) else list()
}


map_chr <- function(f, ...) {
  res <- map_simplify(f, ...)
  storage.mode(res) <- "character"
  res
}


map_logi <- function(f, ...) {
  res <- map_simplify(f, ...)
  storage.mode(res) <- "logical"
  res
}


map_num <- function(f, ...) {
  res <- map_simplify(f, ...)
  storage.mode(res) <- "numeric"
  res
}


map_simplify <- function(f, ...) {
  res <- map(f, ...)
  if (length(res)) simplify2array(res, higher = TRUE) else res
}


match_class <- function(object, choices) {
  f <- function(x, ...) UseMethod("f")
  f.default <- function(x, ...) NA_character_
  for (choice in choices) {
    assign(paste0("f.", choice), eval(substitute(function(x, ...) choice)))
  }
  f(object)
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


missing_names <- function(x, data) {
  x[!(x %in% names(data))]
}


nvars <- function(x, model) {
  stopifnot(is(x, "ModelFrame"))
  model <- getMLObject(model, "MLModel")
  switch(model@predictor_encoding,
         "model.matrix" =
           ncol(model.matrix(x[1, , drop = FALSE], intercept = FALSE)),
         "terms" = {
           x_terms <- attributes(terms(x))
           nrow(x_terms$factors) - x_terms$response - length(x_terms$offset)
         })
}


params <- function(envir) {
  args <- as.list(envir)
  is_missing <- map_logi(function(x) is.symbol(x) && !nzchar(x), args)
  if (any(is_missing)) {
    missing <- names(args)[is_missing]
    stop(label_items("missing values for required argument", missing))
  }
  args[!map_logi(is.null, args)]
}


push <- function(x, object, ...) {
  UseMethod("push")
}


push.TrainBit <- function(x, object, ...) {
  stopifnot(is(object, "MLModelFit"))
  obj_bits <- (if (isS4(object)) object@mlmodel else object$mlmodel)@trainbits
  trainbits <- ListOf(c(x, obj_bits))
  names(trainbits) <- paste0("TrainStep", seq(trainbits))
  if (isS4(object)) {
    object@mlmodel@trainbits <- trainbits
  } else {
    object$mlmodel@trainbits <- trainbits
  }
  object
}


requireModelNamespaces <- function(packages) {
  available <- map_logi(requireNamespace, packages, quietly = TRUE)
  if (!all(available)) {
    missing <- packages[!available]
    stop(label_items("model requires the installation of package", missing),
         call. = FALSE)
  }
  invisible(available)
}


sample_params <- function(x, size = NULL, replace = FALSE) {
  stopifnot(is.list(x))

  n <- length(x)
  if (n == 0) return(tibble())

  varnames <- paste0("Var", seq(x))
  x_names <- names(x)
  if (!is.null(x_names)) {
    is_nzchar <- nzchar(x_names)
    varnames[is_nzchar] <- x_names[is_nzchar]
  }
  names(x) <- varnames

  max_size <- prod(lengths(x))
  if (is.null(size)) size <- max_size
  if (!replace) size <- min(size, max_size)

  grid <- as_tibble(matrix(nrow = 0, ncol = n, dimnames = list(NULL, names(x))))
  iter <- 0
  while (nrow(grid) < size && iter < 100) {
    iter <- iter + 1
    new_grid <- as_tibble(map(sample, x, size = size, replace = TRUE))
    grid <- rbind(grid, new_grid)
    if (!replace) grid <- unique(grid)
  }

  head(grid, size)
}


seq_boot <- function(src, dest) {
  indices <- seq_len(nrow(src))
  pad_size <- nrow(dest) - nrow(src)
  if (pad_size >= 0) {
    c(indices, sample(indices, pad_size, replace = TRUE))
  } else {
    sample(indices, nrow(dest))
  }
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


set_model_names <- function(x) {
  name <- "Model"
  level_names <- list()
  for (i in seq(x)) {
    if (is.null(x[[i]][[name]])) x[[i]][[name]] <- rep(name, nrow(x[[i]]))
    x[[i]][[name]] <- as.factor(x[[i]][[name]])
    arg_name <- names(x)[i]
    level_names[[i]] <- if (!is.null(arg_name) && nzchar(arg_name)) {
      rep(arg_name, nlevels(x[[i]][[name]]))
    } else {
      levels(x[[i]][[name]])
    }
  }
  level_names <- level_names %>% unlist %>% make.unique %>% relist(level_names)

  for (i in seq(x)) levels(x[[i]][[name]]) <- level_names[[i]]

  x
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


strata.BinomialVariate <- function(object, ...) {
  as.numeric(object)
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
  var_name <- info$variable[info$role == "case_stratum"]
  if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) var_name else
      stop("multiple strata variables specified")
}


switch_class <- function(EXPR, ...) {
  blocks <- eval(substitute(alist(...)))
  eval.parent(blocks[[match_class(EXPR, names(blocks))]])
}


unnest <- function(data) {
  stopifnot(is(data, "data.frame"))
  df <- data.frame(row.names = seq_len(nrow(data)))
  for (name in names(data)) {
    x <- data[[name]]
    if (length(dim(x)) > 1) {
      x <- if (is.data.frame(x)) unnest(x) else as.data.frame(as(x, "matrix"))
      name <- paste0(name, ".", names(x))
    }
    df[name] <- x
  }
  df
}


#################### Exception Handling ####################


DomainError <- function(value, ...) {
  errorCondition(message = paste0(...),
                 call = sys.call(-1),
                 value = value,
                 class = "DomainError")
}


depwarn <- function(old, new, expired = FALSE) {
  ifelse(expired, stop, warning)(old, ";\n", new, call. = FALSE)
}


warn <- function(...) {
  warning(..., call. = FALSE)
}
