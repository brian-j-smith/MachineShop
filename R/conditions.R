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


LocalWarning <- function(..., value = NULL) {
  warningCondition(
    message = paste0(...),
    value = value,
    class = "LocalWarning"
  )
}


TypeError <- function(value, expected, subject = character(), call = FALSE) {
  errorCondition(
    message = paste0(
      "Expected ", subject, if (length(subject)) " to be ",
      as_string(expected, conj = "or"), "; got ", class1(value), " instead."
    ),
    call = select_call(call, sys.call(-1)),
    class = "TypeError"
  )
}


Warning <- function(..., value = NULL, call = FALSE) {
  warningCondition(
    message = paste0(...),
    value = value,
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


.throw.LocalWarning <- function(x, immediate = FALSE, ...) {
  warning(x$message, call. = FALSE, immediate. = immediate)
  x$value
}


.throw.warning <- function(x, call, parent_call, times = Inf, ...) {
  x$call <- select_call(call, parent_call, conditionCall(x))
  if (is.call(x$call) && is.finite(times)) {
    name <- x$call[[1]]
    times <- min(times, MachineShop_global$throw_times[[name]])
    MachineShop_global$throw_times[[name]] <- times - 1
  }
  if (times > 0) {
    if (match("warning", class(x)) > 1) {
      x$message <- paste0(class1(x), ": ", x$message)
    }
    warning(x)
  }
  x$value
}


select_call <- function(x, parent_call = NULL, last_call = NULL) {
  if (isTRUE(x)) {
    if (is.null(last_call)) parent_call else last_call
  } else if (is.call(x)) {
    x
  }
}


#################### Checks ####################


check_array <- function(
  x, type, size = integer(), nonempty = TRUE, na.fail = TRUE
) {
  tryCatch(
    {
      storage.mode(x) <- type
      result <- x
      n <- length(size)
      if (n > 0 && (ndim(x) != n || any(na.omit(size(x) != size)))) {
        msg <- if (identical(size, 1)) {
          "a scalar"
        } else if (n == 1) {
          paste("a vector of length", size)
        } else {
          paste(if (n == 2) "a matrix" else "an array", "of dimension",
                paste(size, collapse = "x"))
        }
        DomainError(x, paste("must be", msg))
      } else if (nonempty && is_empty(x)) {
        DomainError(x, "must be non-empty")
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
    },
    error = function(cond) TypeError(x, type)
  )
}


check_assignment <- function(x, value = x) {
  msg <- paste0(" '", deparse1(substitute(x)), "' value.\n")
  if (is(value, "error")) {
    value$message <- paste0("Failed to assign", msg, value$message)
  } else if (is(value, "condition")) {
    value$message <- paste0("Issue in assigning", msg, value$message)
  }
  value
}


check_censoring <- function(x, types, ...) {
  type <- attr(x, "type")
  if (!(type %in% types)) {
    types <- paste0("'", types, "'")
    Error("Expected survival data censoring type to be ",
          as_string(types, conj = "or"), "; got '", type, "' instead.")
  } else x
}


check_character <- function(x, ...) {
  check_array(x, type = "character", ...)
}


check_const_setting <- function(x, name) {
  if (!identical(x, x <- .global_defaults[[name]])) {
    LocalWarning("MachineShop '", name, "' setting cannot be changed.",
                 value = x)
  } else x
}


check_equal_weights <- function(x) {
  if (length(x) && any(diff(x) != 0)) {
    Warning("Model weights are not supported and will be ignored.", value = x)
  } else x
}


check_grid <- function(x) {
  if (is(x, "numeric")) {
    TuningGrid(x)
  } else if (identical(x, "TuningGrid") || identical(x, TuningGrid)) {
    TuningGrid()
  } else if (is(x, "TuningGrid")) {
    x
  } else {
    DomainError(x, "must be one or more positive integers or a ",
                   "TuningGrid function, function name, or object")
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
  tryCatch(
    match.arg(x, choices),
    error = function(cond) {
      choices <- paste0("\"", choices, "\"")
      DomainError(x, "must be one of ", as_string(choices, conj = "or"))
    }
  )
}


check_metric <- function(x, convert = FALSE) {
  tryCatch(
    {
      result <- as.MLMetric(x)
      if (convert) result else x
    },
    error = function(cond) {
      DomainError(x, "must be a metrics function or function name")
    }
  )
}


check_metrics <- function(x, convert = FALSE) {
  tryCatch(
    {
      map(as.MLMetric, c(x))
      if (convert) vector_to_function(x, "metric") else x
    },
    error = function(cond) {
      DomainError(x, "must be a metrics function, function name, ",
                     "or vector of these")
    }
  )
}


check_numeric <- function(
  x, bounds = c(-Inf, Inf), include = TRUE,
  type = c("numeric", "double", "integer"), ...
) {
  result <- check_array(x, type = match.arg(type), ...)
  if (is(result, "error")) {
    return(result)
  }

  include <- rep_len(include, 2)
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


check_optim_bounds <- function(x) {
  if (is_empty(x)) {
    Error("No set of bounded tuning parameters to optimize.")
  } else x
}


check_packages <- function(x) {
  paren_pos <- regexpr("\\(([^)]*)\\)", x)
  paren_len <- attr(paren_pos, "match.length")

  end_pos <- ifelse(paren_pos > 0, paren_pos - 1, nchar(x))
  pkg_names <- trimws(substr(x, 1, end_pos))

  check <- function(failures, msg, pkgs_fun) {
    if (any(failures)) {
      x <- x[failures]
      pkg_names <- pkg_names[failures]
      Error("Call ", note_items(msg, x), ".\n",
            "To address this issue, try running ", pkgs_fun, "(",
            deparse1(pkg_names), ").")
    }
  }

  installed <- map("logi", requireNamespace, pkg_names, quietly = TRUE)
  result <- check(!installed, "requires prior installation of package{?s}: ",
                  "install.packages")
  if (is(result, "error")) return(result)

  end_pos <- paren_pos + paren_len - 2
  compat_versions <- strsplit(substr(x, paren_pos + 1, end_pos), " ")

  compatible <- map("logi", function(pkg_name, compat_version) {
    if (length(compat_version) == 2) {
      version <- packageVersion(pkg_name)
      eval(call(compat_version[1], version, compat_version[2]))
    } else TRUE
  }, pkg_names, compat_versions)
  result <- check(!compatible, "requires updated package version{?s}: ",
                  "update.packages")
  if (is(result, "error")) return(result)

  x
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


#################### Deprecations ####################


#' Deprecated Functions
#'
#' Functions that have been deprecated and will be removed in a future version
#' of the package.
#'
#' @name deprecated
#' @rdname deprecated
#'
#' @param ... arguments passed to non-deprecated equivalent.
#'
NULL


#' @rdname deprecated
#'
#' @details
#' Use \code{\link[=ModelSpecification]{ModelSpecification()}} instead of
#' \code{ModeledInput()}.
#'
ModeledInput <- function(...) {
  throw(DeprecatedCondition(
    "ModeledInput()", "ModelSpecification()",
    expired = Sys.Date() >= "2022-06-01"
  ))
  ModelSpecification(...)
}


#' @rdname deprecated
#'
#' @details
#' Use \code{\link[=ppr]{ppr()}} instead of \code{rpp()}.
#'
rpp <- function(...) {
  throw(DeprecatedCondition(
    "rpp()", "ppr()", expired = Sys.Date() >= "2022-06-01"
  ))
  ppr(...)
}


dep_modeledinput <- function(x, class, dots) {
  throw(DeprecatedCondition(
      class(x), "a ModelSpecification", expired = Sys.Date() >= "2022-07-01"
  ), call = FALSE)
  dots$model <- as.MLModel(x)
  do.call(ModelSpecification, c(list(as(x, class)), dots))
}
