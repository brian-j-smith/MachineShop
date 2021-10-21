#################### Constructors ####################


DeprecatedCondition <- function(old, new, expired = FALSE, call = FALSE) {
  make_cond <- function(f, class) {
    f(message = paste0(old, " is deprecated; use ", new, " instead."),
      call = select_call(call, sys.call(-2)),
      class = class)
  }
  if (expired) {
    make_cond(errorCondition, "DeprecatedError")
  } else {
    make_cond(warningCondition, "DeprecatedWarning")
  }
}


DomainError <- function(value, ..., call = FALSE) {
  errorCondition(
    message = paste0("Value ", ..., "."),
    call = select_call(call, sys.call(-1)),
    value = value,
    class = "DomainError"
  )
}


Error <- function(..., call = FALSE) {
  errorCondition(
    message = paste0(...),
    call = select_call(call, sys.call(-1))
  )
}


LocalError <- function(...) {
  errorCondition(
    message = paste0(...),
    class = "LocalError"
  )
}


LocalWarning <- function(...) {
  warningCondition(
    message = paste0(...),
    class = "LocalWarning"
  )
}


TypeError <- function(value, expected, subject = character(), call = FALSE) {
  errorCondition(
    message = paste0("Expected ", subject, if (length(subject)) " to be ",
                     toString(expected, conj = "or"), "; got ", class1(value),
                     " instead."),
    call = select_call(call, sys.call(-1)),
    class = "TypeError"
  )
}


Warning <- function(..., call = FALSE) {
  warningCondition(
    message = paste0(...),
    call = select_call(call, sys.call(-1))
  )
}


#################### Throw ####################


throw <- function(x, call = TRUE, ...) {
  .throw(x, call = call, parent_call = sys.call(-1), ...)
}


.throw <- function(x, ...) {
  UseMethod(".throw")
}


.throw.default <- function(x, ...) {
  x
}


.throw.error <- function(x, call, parent_call, ...) {
  if (match("error", class(x)) > 1) {
    x$message <- paste0(class1(x), ": ", x$message)
  }
  x$call <- select_call(call, parent_call, conditionCall(x))
  stop(x)
}


.throw.LocalError <- function(x, ...) {
  stop(x$message, call. = FALSE)
}


.throw.LocalWarning <- function(x, ...) {
  warning(x$message, call. = FALSE)
}


.throw.warning <- function(x, call, parent_call, times = Inf, ...) {
  if (is.call(parent_call) && is.finite(times)) {
    name <- parent_call[[1]]
    times <- min(MachineShop_global$throw_times[[name]], times)
    MachineShop_global$throw_times[[name]] <- times - 1
  }
  if (times > 0) {
    if (match("warning", class(x)) > 1) {
      x$message <- paste0(class1(x), ": ", x$message)
    }
    x$call <- select_call(call, parent_call, conditionCall(x))
    warning(x)
  }
}


select_call <- function(x, parent_call = NULL, last_call = NULL) {
  if (isTRUE(x)) {
    if (is.null(last_call)) parent_call else last_call
  } else if (is.call(x)) {
    x
  }
}


#################### Checks ####################


check_array <- function(x, type, size = NULL, na.fail = TRUE) {
  result <- try({
    storage.mode(x) <- type
    x
  }, silent = TRUE)

  n <- length(size)
  if (is(result, "try-error")) {
    TypeError(x, type)
  } else if (n > 0 && (ndim(x) != n || any(na.omit(size(x) != size)))) {
    msg <- if (identical(size, 1)) {
      "a scalar"
    } else if (n == 1) {
      paste("a vector of length", size)
    } else {
      paste(if (n == 2) "a matrix" else "an array", "of dimension",
            paste(size, collapse = "x"))
    }
    DomainError(x, paste("must be", msg))
  } else if (na.fail && anyNA(result)) {
    msg <- paste0(
      c("", "must be ", paste("non-missing", type)),
      if (length(result) == 1) c("", "a ", "") else c("elements ", "", "s"),
      collapse = ""
    )
    DomainError(result, msg)
  } else {
    result
  }
}


check_assignment <- function(x, value = x) {
  if (is(value, "error")) {
    value$message <- paste0("Failed to assign '", deparse1(substitute(x)),
                            "' value.\n", value$message)
  }
  value
}


check_censoring <- function(x, types, ...) {
  type <- attr(x, "type")
  if (!(type %in% types)) {
    types <- paste0("'", types, "'")
    Error("Expected survival data censoring type to be ",
          toString(types, conj = "or"), "; got '", type, "' instead.")
  } else x
}


check_character <- function(x, ...) {
  check_array(x, type = "character", ...)
}


check_const_setting <- function(x, name) {
  if (!identical(x, x <- .global_defaults[[name]])) {
    throw(LocalWarning("MachineShop '", name, "' setting cannot be changed."))
  }
  x
}


check_equal_weights <- function(x) {
  if (length(x) && any(diff(x) != 0)) {
    Warning("Model weights are not supported and will be ignored.")
  }
}


check_grid <- function(x) {
  if (is(x, "numeric")) {
    Grid(x)
  } else if (identical(x, "Grid") || identical(x, Grid)) {
    Grid()
  } else if (is(x, "Grid")) {
    x
  } else {
    DomainError(x, "must be one or more positive integers or a Grid function, ",
                   "function name, or object")
  }
}


check_integer <- function(x, bounds = c(-Inf, Inf), include = TRUE, ...) {
  include <- include & is.finite(bounds)
  check_numeric(x, bounds = bounds, include = include, type = "integer", ...)
}


check_logical <- function(x, ...) {
  check_array(x, type = "logical", ...)
}


check_match <- function(x, choices) {
  tryCatch(match.arg(x, choices), error = function(e) {
    choices <- paste0("\"", choices, "\"")
    DomainError(x, "must be one of ", toString(choices, conj = "or"))
  })
}


check_metric <- function(x, convert = FALSE) {
  result <- try(get_MLMetric(x), silent = TRUE)
  if (is(result, "try-error")) {
    DomainError(x, "must be a metrics function or function name")
  } else if (convert) {
    result
  } else {
    x
  }
}


check_metrics <- function(x, convert = FALSE) {
  result <- try(map(get_MLMetric, c(x)), silent = TRUE)
  if (is(result, "try-error")) {
    DomainError(x, "must be a metrics function, function name, ",
                   "or vector of these")
  } else if (convert) {
    vector_to_function(x, "metric")
  } else {
    x
  }
}


check_numeric <- function(
  x, bounds = c(-Inf, Inf), include = TRUE,
  type = c("numeric", "double", "integer"), ...
) {
  result <- check_array(x, type = match.arg(type), ...)
  if (is(result, "error")) {
    return(result)
  }

  include <- rep(include, length = 2)
  ops <- paste0(c(">", "<"), ifelse(include, "=", ""))
  values <- na.omit(c(result))
  inbounds <- function(op, bound) all(do.call(op, list(values, bound)))
  if (!(inbounds(ops[1], bounds[1]) && inbounds(ops[2], bounds[2]))) {
    msg <- paste0(if (length(result) > 1) "elements ", "must be ",
                  paste(ops, bounds, collapse = " and "))
    DomainError(result, msg)
  } else {
    result
  }
}


check_stat <- function(x, convert = FALSE) {
  result <- try({
    stat <- fget(x)
    stat(1:5)
  }, silent = TRUE)
  if (!is.numeric(result) || length(result) != 1) {
    DomainError(x, "must be a statistic function or function name")
  } else if (convert) {
    stat
  } else {
    x
  }
}


check_stats <- function(x, convert = FALSE) {
  result <- try({
    stats <- vector_to_function(x, "stat")
    stats(1:5)
  }, silent = TRUE)
  if (!is.numeric(result)) {
    DomainError(x, "must be a statistics function, function name, ",
                   "or vector of these")
  } else if (convert) {
    stats
  } else {
    x
  }
}


check_weights <- function(x, along) {
  n <- length(along)
  if (is.null(x)) {
    rep(1, n)
  } else {
    check_numeric(x, bounds = c(0, Inf), include = c(TRUE, FALSE), size = n)
  }
}
