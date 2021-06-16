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
                     toString(expected, conj = "or"), "; got ", class(value)[1],
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


throw <- function(x, call = TRUE) {
  .throw(x, call = call, parent_call = sys.call(-1))
}


.throw <- function(x, ...) {
  UseMethod(".throw")
}


.throw.default <- function(x, ...) {
  x
}


.throw.error <- function(x, call, parent_call, ...) {
  if (match("error", class(x)) > 1) {
    x$message <- paste0(class(x)[1], ": ", x$message)
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


.throw.warning <- function(x, call, parent_call, ...) {
  if (match("warning", class(x)) > 1) {
    x$message <- paste0(class(x)[1], ": ", x$message)
  }
  x$call <- select_call(call, parent_call, conditionCall(x))
  warning(x)
}


select_call <- function(x, parent_call = NULL, last_call = NULL) {
  if (isTRUE(x)) {
    if (is.null(last_call)) parent_call else last_call
  } else if (is.call(x)) {
    x
  }
}


#################### Checks ####################


check_assignment <- function(x, value = x) {
  if (is(value, "error")) {
    value$message <- paste0("Failed to assign '", substitute(x), "' value.\n",
                            value$message)
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


check_integer <- function(...) {
  check_numeric(..., type = "integer")
}


check_logical <- function(x, scalar = TRUE) {
  result <- try(as.logical(x), silent = TRUE)
  pass <- is.logical(result) &&
    length(result) &&
    (!scalar || length(result) == 1) &&
    !any(is.na(result))
  if (!pass) {
    msg <- "must be one%s logical%s"
    edits <- if (scalar) character(2) else c(" or more", "s")
    DomainError(x, sprintf(msg, edits[1], edits[2]))
  } else result
}


check_match <- function(choices) {
  function(x) {
    tryCatch(match.arg(x, choices), error = function(e) {
      choices <- paste0("\"", choices, "\"")
      DomainError(x, "must be one of ", toString(choices, conj = "or"))
    })
  }
}


check_metrics <- function(x) {
  result <- try(map(get_MLMetric, c(x)), silent = TRUE)
  if (is(result, "try-error")) {
    DomainError(x, "must be a metrics function, function name, ",
                   "or vector of these")
  } else x
}


check_numeric <- function(
  x, bounds = c(-Inf, Inf), include = FALSE, type = c("numeric", "integer"),
  scalar = TRUE
) {
  include <- rep(include, length = 2)
  type <- match.arg(type)

  result <- try(suppressWarnings(as(x, type)), silent = TRUE)
  pass <- is(result, type) &&
    length(result) &&
    (!scalar || length(result) == 1) &&
    !any(is.na(result)) &&
    all((result > bounds[1] | (include[1] & result == bounds[1]))) &&
    all((result < bounds[2] | (include[2] & result == bounds[2])))
  if (!pass) {
    directions <- ifelse(include, c(">=", "<="), c(">", "<"))
    msg <- paste0("must be one%s ", type, "%s ",
                  paste(directions, bounds, collapse = " and "))
    edits <- if (scalar) character(2) else c(" or more", "s")
    DomainError(x, sprintf(msg, edits[1], edits[2]))
  } else result
}


check_stat <- function(x) {
  result <- try(fget(x)(1:5), silent = TRUE)
  if (!is.numeric(result) || length(result) != 1) {
    DomainError(x, "must be a statistic function or function name")
  } else x
}


check_stats <- function(x) {
  result <- try(list_to_function(x)(1:5), silent = TRUE)
  if (!is.numeric(result)) {
    DomainError(x, "must be a statistics function, function name, ",
                   "or vector of these")
  } else x
}
