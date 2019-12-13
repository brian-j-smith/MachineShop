#' ModelFrame Class
#'
#' Class for storing data, formulas, and other attributes for \pkg{MachineShop}
#' model fitting.
#'
#' @name ModelFrame
#' @rdname ModelFrame-methods
#'
#' @param x model \code{\link{formula}} or \code{\link{matrix}} of predictor
#'   variables.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} or an object that can be converted
#'   to one.
#' @param na.rm logical indicating whether to remove cases with \code{NA} values
#'   for any of the model variables.
#' @param offsets numeric vector, matrix, or data frame of values to be added
#'   with a fixed coefficient of 1 to linear predictors in compatible regression
#'   models.
#' @param weights vector of case weights [default: equal].
#' @param strata vector of resampling stratification levels [default: none].
#' @param ... arguments passed to other methods.
#'
#' @return \code{ModelFrame} class object that inherits from \code{data.frame}.
#'
#' @seealso  \code{\link{fit}}, \code{\link{resample}}, \code{\link{response}},
#' \code{\link{SelectedModelFrame}}
#'
#' @examples
#' mf <- ModelFrame(ncases / (ncases + ncontrols) ~ agegp + tobgp + alcgp,
#'                  data = esoph, weights = with(esoph, ncases + ncontrols))
#' gbm_fit <- fit(mf, model = GBMModel)
#' varimp(gbm_fit)
#'
ModelFrame <- function(x, ...) {
  UseMethod("ModelFrame")
}


ModelFrame.data.frame <- function(x, ...) {
  casenames <- x[["(casenames)"]]
  weights <- x[["(weights)"]]
  strata <- x[["(strata)"]]
  x[c("(casenames)", "(weights)", "(strata)")] <- NULL
  model_terms <- terms(lapply(names(x)[-1], as.name), names(x)[1],
                       all_numeric = all(sapply(x[-1], is.numeric)))
  ModelFrame(model_terms, x, na.rm = FALSE,
             casenames = if (is.null(casenames)) rownames(x) else casenames,
             weights = weights, strata = strata)
}


#' @rdname ModelFrame-methods
#'
ModelFrame.formula <- function(x, data, na.rm = TRUE, weights = NULL,
                               strata = NULL, ...) {
  invalid_calls <- setdiff(inline_calls(predictors(x)), valid_predictor_calls)
  if (length(invalid_calls)) {
    stop(
      plural_suffix("unsupported predictor variable function", invalid_calls),
      ": ", toString(invalid_calls),
      "; use a recipe or include transformed predictors in the data frame"
    )
  }

  data <- as.data.frame(data)
  model_terms <- terms(x, data = data)
  data[[deparse(response(model_terms))]] <- response(model_terms, data)

  ModelFrame(model_terms, data, na.rm = na.rm, casenames = rownames(data),
             weights = weights, strata = strata, ...)
}


#' @rdname ModelFrame-methods
#'
ModelFrame.matrix <- function(x, y = NULL, na.rm = TRUE, offsets = NULL,
                              weights = NULL, strata = NULL, ...) {
  data <- as.data.frame(x)
  colnames(x) <- names(data)
  if (!is.null(offsets)) {
    offsets <- if (is.vector(offsets, "numeric")) {
      data.frame(offset = offsets)
    } else if (is.matrix(offsets)) {
      if (is.null(colnames(offsets))) {
        colnames(offsets) <- rep("offset", ncol(offsets))
      }
      data.frame(offsets)
    } else if (!is.data.frame(offsets)) {
      stop("'offsets' must be a numeric vector, matrix, or data frame")
    }
    data <- data.frame(data, offsets)
    offsets <- tail(names(data), length(offsets))
  }
  model_terms <- terms(x, y, offsets = offsets)
  data[[deparse(response(model_terms))]] <- y

  ModelFrame(model_terms, data, na.rm = na.rm, casenames = rownames(data),
             weights = weights, strata = strata, ...)
}


ModelFrame.ModelFrame <- function(x, na.rm = TRUE, ...) {
  vars <- as.data.frame(Filter(length, list(...)), stringsAsFactors = FALSE)
  names(vars) <- sapply(names(vars), function(x) paste0("(", x, ")"))
  x[names(vars)] <- vars
  if (na.rm) na.omit(x) else x
}


ModelFrame.recipe <- function(x, ...) {
  x <- prep(x)
  data <- juice(x)

  info <- summary(x)

  var_name <- info$variable[info$role == "case_weight"]
  weights <- if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) data[[var_name]] else
      stop("multiple case weights specified")

  var_name <- info$variable[info$role == "case_stratum"]
  strata <- if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) data[[var_name]] else
      stop("multiple strata variables specified")

  model_terms <- terms(x)
  data[[deparse(response(model_terms))]] <- response(model_terms, data)

  ModelFrame(model_terms, data, na.rm = FALSE,
             casenames = if (is.null(data[["(casenames)"]])) rownames(data),
             weights = weights, strata = strata)
}


ModelFrame.terms <- function(x, data, ...) {
  data[all.vars(model_formula(x))] %>%
    structure(terms = x, class = c("ModelFrame", class(data))) %>%
    ModelFrame(...)
}


#################### ModelFrame Formulas ####################


formula.ModelFrame <- function(x, ...) {
  model_formula(terms(x))
}


inline_calls <- function(x) {
  if (is.call(x)) {
    call_name <- as.character(x[[1]])
    unique(c(call_name, unlist(lapply(x[-1], inline_calls))))
  }
}


model_formula <- function(x) {
  x <- formula(x)
  response <- response(x)
  if (!is.null(response)) x[[2]] <- as.name(deparse(response))
  x
}


valid_predictor_calls <- c(
  "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round", "signif",
  "exp", "log", "expm1", "log1p", "cos", "sin", "tan", "cospi", "sinpi",
  "tanpi", "acos", "asin", "atan",
  "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
  "lgamma", "gamma", "digamma", "trigamma",
  "+", "-", "*", "/", "^", "%%", "%/%",
  "&", "|", "!",
  "==", "!=", "<", "<=", ">=", ">",
  "%in%", "(", ".", ":", "I", "offset"
)


#################### ModelFrame Terms ####################


terms.formula <- function(x, ...) {
  structure(
    stats::terms.formula(x, ...),
    .Environment = asNamespace("MachineShop"),
    class = c("FormulaTerms", "terms", "formula")
  )
}


terms.list <- function(x, y = NULL, intercept = TRUE, all_numeric = FALSE,
                       ...) {
  if (is.character(y)) y <- as.name(y)
  has_y <- 1L - is.null(y)

  is_names <- sapply(x, is.name)
  name_inds <- which(is_names)
  noname_inds <- which(!is_names)

  x_char <- character(length(x))
  x_char[name_inds] <- as.character(x[name_inds])
  x_char[noname_inds] <- vapply(x[noname_inds], deparse, character(1))

  valid_calls <- sapply(x[noname_inds], function(var) {
    is.call(var) && var[[1]] == .("offset")
  })
  if (!all(valid_calls)) stop("non-offset calls in variable specifications")
  offsets <- has_y + noname_inds

  fo <- str2lang(
    paste(
      "y ~",
      if (length(x)) paste(x_char, collapse = "+") else if (intercept) "1",
      if (!intercept) "- 1"
    )
  )
  fo[[2]] <- y

  class <- if (all_numeric) {
    x_char[name_inds] <- sapply(x[name_inds], as.character)
    "DesignTerms"
  } else {
    "FormulaTerms"
  }

  var_names <- c(if (has_y) deparse(y), x_char)
  label_names <- x_char[name_inds]

  if (class == "DesignTerms") {
    factors <- cbind(as.integer(var_names %in% label_names))
    rownames(factors) <- var_names
  } else {
    factors <- matrix(0L, length(var_names), length(label_names),
                      dimnames = list(var_names, label_names))
    term_match <- match(var_names, label_names, nomatch = 0L)
    factors[cbind(var_names[term_match > 0], label_names[term_match])] <- 1L
  }

  structure(
    fo,
    variables = as.call(c(.(list), y, x)),
    offset = if (length(offsets)) offsets,
    factors = factors,
    term.labels = label_names,
    order = rep(1L, length(label_names)),
    intercept = as.integer(intercept),
    response = has_y,
    .Environment = asNamespace("MachineShop"),
    class = c(class, "terms", "formula")
  )
}


terms.matrix <- function(x, y = NULL, offsets = NULL, ...) {
  stopifnot(is.character(colnames(x)))
  stopifnot(!anyDuplicated(colnames(x)))

  labels <- colnames(x)
  response <- if (!is.null(y)) make.unique(c(labels, "y"))[length(labels) + 1]
  labels <- lapply(labels, as.name)

  if (length(offsets)) {
    offsets <- paste0("offset(", offsets, ")")
    labels <- c(labels, lapply(offsets, str2lang))
  }

  terms(labels, response, all_numeric = is.numeric(x))
}


terms.ModelFrame <- function(x, ...) {
  attr(x, "terms")
}


terms.recipe <- function(x, original = FALSE, ...) {
  info <- summary(x, original = original)

  first <- function(x) head(x, 1)
  get_vars <- function(roles = NULL, types = NULL) {
    is_match <- by(info, info$variable, function(split) {
      valid_types <- if (is.null(types)) TRUE else any(types %in% split$type)
      all(roles %in% split$role) && valid_types
    })
    names(is_match)[is_match]
  }

  binom <- c(
    count = first(get_vars(c("binom_x", "outcome"), "numeric")),
    size = first(get_vars(c("binom_size", "outcome"), "numeric"))
  )
  surv <- c(
    time = first(get_vars(c("surv_time", "outcome"), "numeric")),
    event = first(get_vars(c("surv_event", "outcome"), c("logical", "numeric")))
  )
  matrix <- setdiff(get_vars("outcome", "numeric"), c(binom, surv))
  if (length(matrix) == 1) matrix <- character()
  other <- setdiff(get_vars("outcome"), c(binom, surv, matrix))

  have_outcome <- as.logical(lengths(list(binom, surv, matrix, other)))
  if (sum(have_outcome) > 1 || length(other) > 1) {
    stop("specified outcome is not a single variable, binomila variable with ",
         "roles 'binom_x' and 'binom_size', survival variables with ",
         "roles 'surv_time' and 'surv_event', or multiple numeric variables")
  }

  outcome <- if (length(other)) {
    other
  } else if (!is.na(surv["time"])) {
    x <- call("Surv", as.name(surv["time"]))
    if (!is.na(surv["event"])) x[[3]] <- as.name(surv["event"])
    x
  } else if (length(surv)) {
    stop("survival outcome role 'surv_event' specified without 'surv_time'")
  } else if (!any(is.na(binom[c("count", "size")]))) {
    call("BinomialVariate", as.name(binom["count"]), as.name(binom["size"]))
  } else if (length(binom)) {
    stop("binomial outcome must have 'binom_x' and 'binom_size' roles")
  } else if (length(matrix)) {
    as.call(c(.(cbind), lapply(matrix, as.name)))
  }

  is_predictor <- info$role == "predictor"
  predictors <- lapply(info$variable[is_predictor], as.name)
  all_numeric <- all(info$type[is_predictor] == "numeric")

  offsets <- get_vars("pred_offset", "numeric")
  if (length(offsets)) {
    offsets <- paste0("offset(", offsets, ")")
    predictors <- c(predictors, lapply(offsets, str2lang))
  }

  terms(predictors, outcome, all_numeric = all_numeric)
}


#################### ModelFrame Design Matrices ####################


model.matrix.DesignTerms <- function(object, data, ...) {
  data <- data[labels(object)]
  assign <- seq_len(ncol(data))
  if (attr(object, "intercept")) {
    data <- cbind("(Intercept)" = 1, data)
    assign <- c(0, assign)
  }
  structure(as.matrix(data), assign = assign)
}


model.matrix.FormulaTerms <- function(object, data, ...) {
  model.matrix.default(delete.response(object), as.data.frame(data), ...)
}


model.matrix.ModelFrame <- function(object, intercept = NULL, ...) {
  model_terms <- terms(object)
  if (!is.null(intercept)) {
    attr(model_terms, "intercept") <- as.integer(intercept)
  }
  model.matrix(model_terms, object, ...)
}


model.matrix.SelectedModelFrame <- function(object, ...) {
  stop("cannot create a design matrix from a ", class(object))
}


model.offset <- function(x) {
  stopifnot(is(x, "ModelFrame"))
  keep <- 1 + c(0, attr(terms(x), "offset"))
  offsets <- eval(attr(terms(x), "variables")[keep], x)
  Reduce("+", offsets)
}


#################### ModelFrame Preprocessing ####################


preprocess <- function(x, newdata = NULL) {
  ModelFrame(delete.response(terms(x)), predictors(x, newdata), na.rm = FALSE)
}
