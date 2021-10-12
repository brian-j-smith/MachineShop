utils::globalVariables(c("x", "y"))


.onLoad <- function(libname, pkgname) {
  registerDoSEQ()
  invisible()
}


#' Quote Operator
#'
#' Shorthand notation for the \code{\link[base:substitute]{quote}} function.
#' The quote operator simply returns its argument unevaluated and can be applied
#' to any \R expression.  Useful for calling model constructors with quoted
#' parameter values that are defined in terms of \code{nobs}, \code{nvars}, or
#' \code{y}.
#'
#' @name quote
#'
#' @param expr any syntactically valid \R expression.
#'
#' @return
#' The quoted (unevaluated) expression.
#'
#' @seealso \code{\link[base:substitute]{quote}}
#'
#' @examples
#' ## Stepwise variable selection with BIC
#' glm_fit <- fit(sale_amount ~ ., ICHomes, GLMStepAICModel(k = .(log(nobs))))
#' varimp(glm_fit)
#'
. <- function(expr) {
  eval(substitute(quote(expr)))
}


attach_objects <- function(
  what, pos = 2L, name = deparse1(substitute(what), backtick = FALSE)
) {
  make_attach <- attach
  make_attach(what, pos, name, warn.conflicts = FALSE)
  do.call(on.exit, list(substitute(detach(name))), envir = parent.frame())
}


class1 <- function(x) {
  class(x)[1]
}


combine_data_frames <- function(x, y = NULL) {
  if (is.null(y)) return(x)
  common_cols <- intersect(names(x), names(y))
  if (!identical(x[common_cols], y[common_cols])) {
    throw(Error("common columns in data frames differ"))
  }
  diff_cols <- setdiff(names(y), common_cols)
  x[diff_cols] <- y[diff_cols]
  x
}


combine_model_slots <- function(models, types) {
  init <- data.frame(type = types, weights = FALSE)
  any_ordered <- any(map_logi(function(model) {
    "ordered" %in% model@response_types
  }, models))
  info_list <- map(function(model) {
    types <- model@response_types
    res <- data.frame(type = types, weights = model@weights)
    factor_match <- match("factor", types, nomatch = 0)
    if (any_ordered && !("ordered" %in% types) && factor_match) {
      res <- rbind(
        res,
        data.frame(type = "ordered", weights = res$weights[factor_match])
      )
    }
    res
  }, models)
  info <- Reduce(function(x, y) {
    res <- merge(x, y, by = "type")
    res$weights <- if (nrow(res)) res$weights.x | res$weights.y else logical()
    res[c("weights.x", "weights.y")] <- NULL
    res
  }, info_list, init)
  list(
    response_types = info$type,
    weights = switch(length(unique(info$weights)) + 1,
                     FALSE, info$weights[1], info@weights)
  )
}


complete_subset <- function(...) {
  is_complete <- complete.cases(...)
  map(function(x) subset(x, is_complete), list(...))
}


deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}


fget <- function(x) {
  f <- get0(x, mode = "function")
  if (is.null(f)) {
    msg <- if (is.character(x)) {
      paste0("function '", x, "' not found")
    } else {
      "invalid function"
    }
    throw(Error(msg))
  }
  f
}


get0 <- function(x, mode = "any") {
  if (is.character(x) && length(x) == 1) {
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


get_MLObject <- function(x, class = c("MLControl", "MLMetric", "MLModel")) {
  class <- match.arg(class)
  x <- get0(x)
  if (is.function(x) && class %in% c("MLControl", "MLModel")) x <- x()
  if (!is(x, class)) throw(TypeError(x, class, "'x'"))
  x
}


get_MLControl <- function(x) {
  get_MLObject(x, class = "MLControl")
}


get_MLMetric <- function(x) {
  get_MLObject(x, class = "MLMetric")
}


get_MLModel <- function(x) {
  get_MLObject(x, class = "MLModel")
}


get_perf_metrics <- function(x, y) {
  generic_name <- "performance"
  classes <- substring(methods(generic_name), nchar(generic_name) + 2)
  class <- match_class(x, classes)
  method <- fget(paste0(generic_name, ".", class))
  metrics <- c(eval(formals(method)$metrics))
  is_defined <- map_logi(function(metric) {
    types <- metricinfo(metric)[[1]]$response_types
    any(map_logi(is, list(x), types$observed) &
          map_logi(is, list(y), types$predicted))
  }, metrics)
  metrics[is_defined]
}


has_grid <- function(object) {
  nrow(object@gridinfo) > 0
}


has_varimp <- function(object) {
  !is.null(body(object@varimp))
}


identical_elements <- function(x, transform = identity, ...) {
  target <- transform(x[[1]])
  compare <- function(current) identical(transform(current), target, ...)
  all(map_logi(compare, x[-1]))
}


is_counting <- function(x) {
  isTRUE(attr(x, "type") == "counting")
}


is_one_element <- function(x, class = "ANY") {
  is.vector(x) && length(x) == 1 && is(x[[1]], class)
}


is_response <- function(y, types) {
  map_logi(function(type) {
    if (type == "binary") {
      is(y, "factor") && nlevels(y) == 2
    } else {
      is(y, type)
    }
  }, types)
}


is_trained <- function(x, ...) {
  UseMethod("is_trained")
}


is_trained.MLModel <- function(x, ...) {
  length(x@train_steps) > 0
}


is_trained.step <- function(x, ...) {
  recipes::is_trained(x)
}


label_items <- function(label, x, n = Inf, add_names = FALSE) {
  item_len <- length(x)
  if (add_names && !is.null(names(x))) x <- paste(names(x), x, sep = " = ")
  items <- if (n < item_len) paste(toString(head(x, n)), "...") else toString(x)
  if (item_len != 1) label <- paste0(label, "s")
  if (item_len) paste0(label, ": ", items) else label
}


make_list_names <- function(x, prefix) {
  old_names <- names(x)
  names(x) <- if (length(x)) {
    if (length(x) > 1) paste0(prefix, ".", seq_along(x)) else prefix
  }
  if (!is.null(old_names)) {
    keep <- nzchar(old_names)
    names(x)[keep] <- old_names[keep]
  }
  names(x)
}


map <- function(FUN, ...) {
  all_args <- all(lengths(list(...)))
  if (all_args) mapply(FUN = FUN, ..., SIMPLIFY = FALSE) else list()
}


map_chr <- function(FUN, ...) {
  res <- map_simplify(FUN, ...)
  storage.mode(res) <- "character"
  res
}


map_logi <- function(FUN, ...) {
  res <- map_simplify(FUN, ...)
  storage.mode(res) <- "logical"
  res
}


map_num <- function(FUN, ...) {
  res <- map_simplify(FUN, ...)
  storage.mode(res) <- "numeric"
  res
}


map_simplify <- function(FUN, ...) {
  res <- map(FUN, ...)
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
    throw(LocalWarning("specified indices not found; using ", indices,
                       " instead"))
  }
  indices
}


max.progress_index <- function(x, ...) {
  attr(x, "max")
}


missing_names <- function(x, data) {
  x[!(x %in% names(data))]
}


new_params <- function(envir, ...) {
  args <- as.list(envir)
  is_missing <- map_logi(function(x) is.symbol(x) && !nzchar(x), args)
  if (any(is_missing)) {
    missing <- names(args)[is_missing]
    throw(Error(label_items("missing values for required argument", missing)))
  }
  c(args[!map_logi(is.null, args)], list(...))
}


new_progress_bar <- function(total, input = NULL, model = NULL, index = 0) {
  if (getDoParName() == "doSEQ") index <- as.numeric(index)
  width <- max(10, round(0.25 * getOption("width")))
  if (!is.null(input)) input <- substr(class1(input), 1, width)
  if (!is.null(model)) {
    model <- substr(get_MLModel(model)@name, 1, width)
  }
  format <- paste(input, "|", model)
  if (index > 0) format <- paste0(index, ": ", format)
  if (getDoParName() %in% c("doSEQ", "doSNOW")) {
    format <- paste(format, "[:bar] :percent | :eta")
  }
  pb <- progress_bar$new(
    format = format,
    total = total,
    clear = TRUE,
    show_after = 0
  )
  if (is(index, "progress_index") && index == 1) {
    pb$message(paste0(names(index), "(", max(index), ")"))
  }
  pb$tick(0)
  pb
}


new_progress_index <- function(names, max) {
  structure(0, names = names, max = max, class = c("progress_index", "numeric"))
}


ndim <- function(x) {
  length(size(x))
}


nvars <- function(x, model) {
  stopifnot(is(x, "ModelFrame"))
  model <- get_MLModel(model)
  res <- switch(model@predictor_encoding,
    "model.frame" = {
      x_terms <- attributes(terms(x))
      nrow(x_terms$factors) - x_terms$response - length(x_terms$offset)
    },
    "model.matrix" = ncol(model.matrix(x[1, , drop = FALSE], intercept = FALSE))
  )
  if (is.null(res)) NA else res
}


push <- function(x, object, ...) {
  UseMethod("push")
}


push.TrainStep <- function(x, object, ...) {
  stopifnot(is(object, "MLModelFit"))
  mlmodel <- if (isS4(object)) object@mlmodel else object$mlmodel
  train_steps <- ListOf(c(x, mlmodel@train_steps))
  names(train_steps) <- paste0("TrainStep", seq_along(train_steps))
  if (isS4(object)) {
    object@mlmodel@train_steps <- train_steps
  } else {
    object$mlmodel@train_steps <- train_steps
  }
  object
}


rand_int <- function(n = 1) {
  sample.int(.Machine$integer.max, n)
}


require_namespaces <- function(packages) {
  paren_pos <- regexpr("\\(([^)]*)\\)", packages)
  paren_len <- attr(paren_pos, "match.length")

  end_pos <- ifelse(paren_pos > 0, paren_pos - 1, nchar(packages))
  package_names <- trimws(substr(packages, 1, end_pos))

  package_errors <- function(x, msg, fix) {
    if (any(x)) {
      items <- packages[x]
      item_names <- package_names[x]
      throw(LocalError("Call ", label_items(msg, items), ".\n",
                       "To address this issue, try running ", fix, "(",
                       deparse1(item_names), ")."))
    }
  }

  installed <- map_logi(requireNamespace, package_names, quietly = TRUE)
  package_errors(!installed, "requires prior installation of package",
                 "install.packages")

  end_pos <- paren_pos + paren_len - 2
  compat_versions <- strsplit(substr(packages, paren_pos + 1, end_pos), " ")

  compatible <- map_logi(function(package_name, compat_version) {
    if (length(compat_version) == 2) {
      version <- packageVersion(package_name)
      eval(call(compat_version[1], version, compat_version[2]))
    } else TRUE
  }, package_names, compat_versions)
  package_errors(!compatible, "requires updated package version",
                 "update.packages")

  invisible(installed & compatible)
}


sample_params <- function(x, size = NULL, replace = FALSE) {
  stopifnot(is.list(x))

  n <- length(x)
  if (n == 0) return(tibble())

  var_names <- paste0("Var", seq_along(x))
  x_names <- names(x)
  if (!is.null(x_names)) {
    is_nzchar <- nzchar(x_names)
    var_names[is_nzchar] <- x_names[is_nzchar]
  }
  names(x) <- var_names

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

  grid <- head(grid, size)
  sortable_types <- c("character", "complex", "Date", "factor", "logical",
                      "numeric")
  is_sortable <- map_logi(function(column) {
    any(map_logi(is, list(column), sortable_types))
  }, grid)
  if (any(is_sortable)) {
    sort_order <- do.call(order, rev(grid[is_sortable]))
    grid <- grid[sort_order, ]
  }

  grid
}


sample_replace <- function(x, inds) {
  if (!is.logical(inds)) {
    old_inds <- inds
    inds <- structure(logical(length(x)), names = names(x))
    inds[old_inds] <- TRUE
  }
  x[inds] <- sample(x[!inds], sum(inds), replace = TRUE)
  x
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
  for (i in seq_along(x)) {
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

  for (i in seq_along(x)) levels(x[[i]][[name]]) <- level_names[[i]]

  x
}


size <- function(x, dim = NULL) {
  res <- dim(x)
  if (is.null(res)) res <- length(x)
  if (is.null(dim)) res else res[dim]
}


stepAIC_args <- function(formula, direction, scope) {
  if (is.null(scope$lower)) scope$lower <- ~ 1
  if (is.null(scope$upper)) scope$upper <- formula[-2]
  formula[-2] <- if (direction == "backward") scope$upper else scope$lower
  list(formula = formula, scope = scope)
}


subset_names <- function(x, select = NULL) {
  indices <- seq_along(x)
  names(indices) <- x
  select <- eval(substitute(select), as.list(indices), parent.frame())
  if (is.character(select)) {
    x <- intersect(x, select)
  } else if (is.numeric(select)) {
    x <- x[select]
  } else if (!is.null(select)) {
    throw(Warning("invalid 'select' value of class ", class1(select)))
  }
  x
}


switch_class <- function(EXPR, ...) {
  blocks <- eval(substitute(alist(...)))
  eval.parent(blocks[[match_class(EXPR, names(blocks))]])
}


toString.character <- function(x, conjunction = NULL, ...) {
  if (!is.null(conjunction)) {
    n <- length(x)
    if (n > 1) x[n] <- paste(conjunction, x[n])
    if (n == 2) x <- paste(x[1], x[2])
  }
  NextMethod()
}


unnest <- function(data) {
  stopifnot(is(data, "data.frame"))
  df <- data.frame(row.names = seq_len(nrow(data)))
  for (name in names(data)) {
    x <- data[[name]]
    if (ndim(x) > 1) {
      x <- if (is.data.frame(x)) unnest(x) else as.data.frame(as(x, "matrix"))
      name <- paste0(name, ".", names(x))
    }
    df[name] <- x
  }
  df
}


vector_to_function <- function(x, type) {
  err_msg <- paste0("'", deparse1(substitute(x)), "' must be a function, ",
                      "function name, or vector of these")
  if (is(x, "MLMetric")) x <- list(x)
  if (is(x, "vector")) {
    x <- as.list(x)
    x_names <- character()
    for (i in seq_along(x)) {
      if (is(x[[i]], "character")) {
        x_name <- x[[i]]
        x[[i]] <- fget(x_name)
      } else if (is(x[[i]], "MLMetric")) {
        x_name <- x[[i]]@name
      } else if (is(x[[i]], "function")) {
        x_name <- type
      } else {
        throw(Error(err_msg))
      }
      name <- names(x)[i]
      x_names[i] <- if (is.null(name) || !nzchar(name)) x_name else name
    }
    names(x) <- make.unique(x_names)
    function(...) unlist(map(function(fun) fun(...), x))
  } else if (is(x, "function")) {
    x
  } else {
    throw(Error(err_msg))
  }
}
