utils::globalVariables(c("x", "y"))


.onLoad <- function(libname, pkgname) {
  registerDoSEQ()
  invisible()
}


#' Quote Operator
#'
#' Shorthand notation for the \code{\link[base:substitute]{quote}} function.
#' The quote operator simply returns its argument unevaluated and can be applied
#' to any \R expression.
#'
#' @name quote
#'
#' @param expr any syntactically valid \R expression.
#'
#' @details
#' Useful for calling \link[=models]{model functions} with quoted parameter
#' values defined in terms of one or more of the following variables.
#' \describe{
#'   \item{\code{nobs}}{number of observations in data to be \link{fit}.}
#'   \item{\code{nvars}}{number of predictor variables.}
#'   \item{\code{y}}{the response variable.}
#' }
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


as_string <- function(x, ...) {
  UseMethod("as_string")
}


as_string.default <- function(x, ...) {
  x <- format(x, trim = TRUE, justify = "none", drop0trailing = TRUE)
  as_string(structure(as.character(x), names = names(x)), ...)
}


as_string.character <- function(x, sep = ", ", conj = character()) {
  x <- trimws(x)
  if (length(conj)) {
    n <- length(x)
    if (n > 1) x[n] <- paste(conj, x[n])
    if (n == 2) x <- paste(x[1], x[2])
  }
  if (length(sep)) paste(x, collapse = sep) else x
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


combine_inputs <- function(x) {
  names(x) <- make_names_along(x, map("char", class, x))
  all_data <- NULL
  for (i in seq_along(x)) {
    data <- as.data.frame(x[[i]])
    if (is.null(all_data)) all_data <- data[NULL]
    same_cols <- intersect(names(all_data), names(data))
    if (!identical(all_data[same_cols], data[same_cols])) {
      throw(Error(
        "Cannot combine ", names(x)[i], " with previous data fames due to ",
        "differences in column values or attributes."
      ))
    }
    diff_cols <- setdiff(names(data), same_cols)
    all_data[diff_cols] <- data[diff_cols]
    x[[i]] <- update(x[[i]], data = data[NULL, , drop = FALSE])
  }
  list(data = all_data, candidates = ListOf(x))
}


combine_model_slots <- function(models, types) {
  init <- data.frame(type = types, weights = FALSE)
  any_ordered <- any(map("logi", function(model) {
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


fget <- function(x, package = character()) {
  throw(check_packages(package))
  if (is.character(x)) {
    x <- paste(c(package, x), collapse = "::")
    err_msg <- paste0("Function '", x, "' not found.")
  } else {
    err_msg <- "Invalid function."
  }
  res <- get0(x, mode = "function")
  if (is.null(res)) throw(Error(err_msg))
  if (length(package)) {
    envir <- environment(res)
    if (!isNamespace(envir) || getNamespaceName(envir) != package) {
      throw(Error("Function is not from the ", package, " package."))
    }
  }
  res
}


get0 <- function(x, mode = "any") {
  if (is.character(x) && length(x) == 1) {
    expr <- str2lang(x)
    if (is.name(expr)) {
      base::get0(x, mode = mode)
    } else if (is.call(expr) && any(expr[[1]] == c("::", ":::"))) {
      base::get0(as.character(expr[[3]]), mode = mode,
                 envir = asNamespace(expr[[2]]))
    }
  } else if (mode %in% c("any", mode(x))) {
    x
  }
}


get_optim_field <- function(object, name = NULL) {
  object <- object@params@optim
  if (is.null(name)) object else slot(object, name)
}


get_perf_metrics <- function(x, y) {
  generic_name <- "performance"
  classes <- substring(methods(generic_name), nchar(generic_name) + 2)
  class <- match_class(x, classes)
  method <- fget(paste0(generic_name, ".", class))
  metrics <- c(eval(formals(method)$metrics))
  is_defined <- map("logi", function(metric) {
    types <- metricinfo(metric)[[1]]$response_types
    any(map("logi", is, list(x), types$observed) &
          map("logi", is, list(y), types$predicted))
  }, metrics)
  metrics[is_defined]
}


has_grid <- function(object) {
  is(object, "TunedInput") || is(object, "TunedModel") ||
    (is(object, "MLModel") && nrow(object@gridinfo))
}


has_varimp <- function(object) {
  !is.null(body(object@varimp))
}


identical_elements <- function(x, transform = identity, ...) {
  target <- transform(x[[1]])
  compare <- function(current) identical(transform(current), target, ...)
  all(map("logi", compare, x[-1]))
}


is_counting <- function(x) {
  isTRUE(attr(x, "type") == "counting")
}


is_empty <- function(x) {
  prod(size(x)) == 0
}


is_one_element <- function(x, class = "ANY") {
  is.vector(x) && length(x) == 1 && is(x[[1]], class)
}


is_optim_method <- function(x, ...) {
  UseMethod("is_optim_method")
}


is_optim_method.default <- function(x, ...) {
  FALSE
}


is_optim_method.MLInput <- function(x, ...) {
  is_optim_method(x@params, ...)
}


is_optim_method.MLModel <- function(x, ...) {
  is_optim_method(x@params, ...)
}


is_optim_method.MLOptimization <- function(x, type = "ANY", ...) {
  is(x, type)
}


is_optim_method.ModelSpecification <- function(x, ...) {
  is_optim_method(x@params, ...)
}


is_optim_method.NullOptimization <- function(x, ...) {
  is_optim_method.default(x, ...)
}


is_optim_method.TrainingParams <- function(x, ...) {
  is_optim_method(x@optim, ...)
}


is_response <- function(x, types) {
  map("logi", function(type) {
    if (type == "binary") {
      is(x, "factor") && nlevels(x) == 2
    } else {
      is(x, type)
    }
  }, types)
}


is_trained <- function(x, ...) {
  UseMethod("is_trained")
}


is_trained.MLModel <- function(x, ...) {
  length(x@steps) > 0
}


is_trained.ModelRecipe <- function(x, ...) {
  recipes::fully_trained(x)
}


is_trained.step <- function(x, ...) {
  recipes::is_trained(x)
}


make_id <- function(prefix = character(), n = 4, sep = ".") {
  suffix <- sample(c(letters, LETTERS, 0:9), n, replace = TRUE)
  paste(c(prefix, paste(suffix, collapse = "")), collapse = sep)
}


make_names_along <- function(x, default = "..", unique = TRUE, sep = ".") {
  if (length(default) == 1) default <- rep_len(default, length(x))
  old_names <- names(x)
  names(x) <- default
  if (!is.null(old_names)) {
    keep <- nzchar(old_names) & !is.na(old_names)
    names(x)[keep] <- old_names[keep]
  }
  if (unique) make_unique(names(x), sep = sep) else names(x)
}


make_names_len <- function(n, prefix) {
  paste0(rep_len(prefix, n), seq_len(n))
}


make_unique <- function(names, sep = ".") {
  sort_order <- order(names)
  counts <- table(names)
  unames <- map(function(count, name) {
    paste0(name, if (count > 1) paste0(sep, seq_len(count)))
  }, counts, names(counts))
  names[sort_order] <- unlist(unames)
  names
}


map <- function(.object, ...) {
  UseMethod("map")
}


map.character <- function(.object, .fun, ...) {
  type <- match.arg(.object, c("character", "complex", "double", "integer",
                               "list", "logical", "numeric", "raw"))
  res <- simplify(map(.fun, ...))
  storage.mode(res) <- type
  res
}


map.function <- function(.object, ...) {
  nonempty <- all(lengths(list(...)))
  if (nonempty) mapply(FUN = .object, ..., SIMPLIFY = FALSE) else list()
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
  if (is_empty(indices)) {
    indices <- names(lookup)[1]
    throw(LocalWarning(
      "Specified indices not found; using ", indices, " instead."
    ))
  }
  indices
}


max.progress_index <- function(x, ...) {
  attr(x, "max")
}


missing_names <- function(x, data) {
  x[!(x %in% names(data))]
}


ndim <- function(x) {
  length(size(x))
}


new_params <- function(envir, ...) {
  args <- c(as.list(envir), ...)
  missing <- map("logi", function(x) is.name(x) && !nzchar(x), args)
  if (any(missing)) {
    throw(Error(note_items(
      "Missing values for required argument{?s}: ", names(args)[missing], "."
    )))
  }
  args[lengths(args) > 0]
}


new_progress_bar <- function(total, object, index = 0) {
  if (getDoParName() == "doSEQ") index <- as.numeric(index)
  width <- max(round(0.25 * console_width()), 10)
  input <- substr(class(as.MLInput(object)), 1, width)
  model <- substr(as.MLModel(object)@name, 1, width)
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
    pb$message(paste0(
      names(index), if (is.finite(max(index))) paste0("(", max(index), ")")
    ))
  }
  pb$tick(0)
  pb
}


new_progress_index <- function(init = 0, max = Inf, name = character()) {
  structure(init, max = max, names = name, class = "progress_index")
}


note_items <- function(
  begin, values, end = character(), add_names = FALSE, add_size = FALSE,
  sep = ", ", conj = character()
) {
  if (add_names && length(names(values))) {
    values <- paste(names(values), values, sep = " = ")
  }
  size <- length(values)
  if (add_size && size) {
    values <- c(
      paste0("[1", if (size > 1) paste0(":", size), "] ", values[1]), values[-1]
    )
  }
  end <- paste0(end, "{qty(size)}")
  pluralize(begin, as_string(values, sep = sep, conj = conj), end)
}


nvars <- function(x, model) {
  stopifnot(is(x, "ModelFrame"))
  model <- as.MLModel(model)
  res <- switch(model@predictor_encoding,
    "model.frame" = {
      x_terms <- attributes(terms(x))
      nrow(x_terms$factors) - x_terms$response - length(x_terms$offset)
    },
    "model.matrix" = ncol(model.matrix(x[1, , drop = FALSE], intercept = FALSE))
  )
  if (is.null(res)) NA else res
}


permute_int <- function(n, size = n) {
  inds <- sample.int(n, size)
  half_size <- size / 2
  inds1 <- head(inds, half_size)
  inds2 <- tail(inds, -half_size)
  res <- data.frame(i = c(inds1, inds2), j = c(inds2, inds1))
  res[order(res$i), ]
}


push <- function(x, object, ...) {
  UseMethod("push")
}


push.TrainingStep <- function(x, object, ...) {
  stopifnot(is(object, "MLModelFit"))
  mlmodel <- if (isS4(object)) object@mlmodel else object$mlmodel
  steps <- ListOf(c(x, mlmodel@steps))
  names(steps) <- make_names_len(length(steps), class(x))
  if (isS4(object)) {
    object@mlmodel@steps <- steps
  } else {
    object$mlmodel@steps <- steps
  }
  object
}


rand_int <- function(n = 1) {
  sample.int(.Machine$integer.max, n)
}


required_packages <- function(object) {
  input <- as.MLInput(object)
  require <- if (is(input, "ModelRecipe")) {
    c("survival", "recipes")
  } else if (is(response(input), "Surv")) {
    "survival"
  }
  union(settings("require"), require)
}


round_int <- function(...) {
  as.integer(round(...))
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


seq_int <- function(...) {
  as.integer(seq(...))
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
  round_int(vals)
}


seq_range <- function(from, by, bounds, length) {
  if (length > 0) {
    to <- min(from + by * (length - 1), bounds[2])
    x <- seq(from, to, length = length)
    x[x >= bounds[1]]
  } else {
    seq(from, length = length)
  }
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
  level_names <- level_names %>% unlist %>% make_unique %>% relist(level_names)

  for (i in seq_along(x)) levels(x[[i]][[name]]) <- level_names[[i]]

  x
}


simplify <- function(x) {
  if (length(x)) simplify2array(x, higher = TRUE) else x
}


size <- function(x, dim = integer()) {
  res <- dim(x)
  if (is.null(res)) res <- length(x)
  if (length(dim)) res[dim] else res
}


stepAIC_args <- function(formula, direction, scope) {
  if (is.null(scope$lower)) scope$lower <- ~ 1
  if (is.null(scope$upper)) scope$upper <- formula[-2]
  formula[-2] <- if (direction == "backward") scope$upper else scope$lower
  list(formula = formula, scope = scope)
}


subset_selected <- function(object, slot_name, id = character()) {
  if (length(id)) {
    keep <- map("char", slot, slot(object, slot_name), "id") %in% id
    slot(object, slot_name) <- slot(object, slot_name)[keep]
  }
  object
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
    throw(Warning("Invalid 'select' value of class ", class1(select), "."))
  }
  x
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
    if (ndim(x) > 1) {
      x <- if (is.data.frame(x)) unnest(x) else as.data.frame(as(x, "matrix"))
      name <- paste0(name, "$", names(x))
    }
    df[name] <- x
  }
  df
}


vector_to_function <- function(x, type) {
  err_msg <- paste0("Value of '", deparse1(substitute(x)),
                    "' must be a function, function name, or vector of these.")
  if (is(x, "MLMetric")) x <- list(x)
  if (is(x, "vector")) {
    x <- as.list(x)
    types <- make_names_len(length(x), type)
    x_names <- character()
    for (i in seq_along(x)) {
      if (is(x[[i]], "character")) {
        x_name <- gsub("^.*[:]{2}(?!.*[:]{2})", "", x[[i]], perl = TRUE)
        x[[i]] <- fget(x[[i]])
      } else if (is(x[[i]], "MLMetric")) {
        x_name <- x[[i]]@name
      } else if (is(x[[i]], "function")) {
        x_name <- types[i]
      } else {
        throw(Error(err_msg))
      }
      name <- names(x)[i]
      x_names[i] <- if (is.null(name) || !nzchar(name)) x_name else name
    }
    names(x) <- make_unique(x_names)
    function(...) unlist(map(function(fun) fun(...), x))
  } else if (is(x, "function")) {
    x
  } else {
    throw(Error(err_msg))
  }
}
