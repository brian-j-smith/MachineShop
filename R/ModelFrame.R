#' ModelFrame Class
#'
#' Class for storing data, formulas, and other attributes for \pkg{MachineShop}
#' model fitting.
#'
#' @name ModelFrame
#' @rdname ModelFrame-methods
#'
#' @param x model \code{\link{formula}} or \code{\link{matrix}} of predictor
#'   variables.  In the case of a formula, arguments \code{weights} and
#'   \code{strata} are evaluated as expressions, whose objects are searched for
#'   first in the accompanying \code{data} environment and, if not found there,
#'   next in the calling environment.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} or an object that can be converted
#'   to one.
#' @param na.rm logical indicating whether to remove cases with \code{NA} values
#'   for any of the model variables.
#' @param offsets numeric vector, matrix, or data frame of values to be added
#'   with a fixed coefficient of 1 to linear predictors in compatible regression
#'   models.
#' @param weights numeric vector of non-negative case weights for the \code{y}
#'   response variable [default: equal weights].
#' @param strata vector of values to use in conducting stratified
#'   \link{resample} estimation of model performance [default: none].
#' @param ... arguments passed to other methods.
#'
#' @return \code{ModelFrame} class object that inherits from \code{data.frame}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{response}},
#' \code{\link{SelectedInput}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' mf <- ModelFrame(ncases / (ncases + ncontrols) ~ agegp + tobgp + alcgp,
#'                  data = esoph, weights = ncases + ncontrols)
#' gbm_fit <- fit(mf, model = GBMModel)
#' varimp(gbm_fit)
#' }
#'
ModelFrame <- function(x, ...) {
  UseMethod("ModelFrame")
}


ModelFrame.data.frame <- function(x, ...) {
  case_names <- x[["(names)"]]
  weights <- x[["(weights)"]]
  strata <- x[["(strata)"]]
  x[c("(names)", "(weights)", "(strata)")] <- NULL
  model_terms <- terms(map(as.name, names(x)[-1]), names(x)[1],
                       all_numeric = all(map_logi(is.numeric, x[-1])))
  ModelFrame(model_terms, x, na.rm = FALSE,
             names = if (is.null(case_names)) rownames(x) else case_names,
             weights = weights, strata = strata)
}


#' @rdname ModelFrame-methods
#'
ModelFrame.formula <- function(
  x, data, na.rm = TRUE, weights = NULL, strata = NULL, ...
) {
  invalid_calls <- setdiff(inline_calls(predictors(x)), settings("RHS.formula"))
  if (length(invalid_calls)) {
    throw(Error(
      label_items("unsupported predictor variable function", invalid_calls),
      "; use a recipe or include transformed predictors in the data frame"
    ))
  }

  data <- as.data.frame(data)

  weights <- eval(substitute(weights), data, parent.frame())
  strata <- eval(substitute(strata), data, parent.frame())

  model_terms <- terms(x, data = data)
  data[[deparse1(response(model_terms))]] <- response(model_terms, data)
  data <- data[all.vars(model_formula(model_terms))]

  ModelFrame(model_terms, data, na.rm = na.rm, names = rownames(data),
             weights = weights, strata = strata, ...)
}


#' @rdname ModelFrame-methods
#'
ModelFrame.matrix <- function(
  x, y = NULL, na.rm = TRUE, offsets = NULL, weights = NULL, strata = NULL, ...
) {
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
      throw(Error("'offsets' must be a numeric vector, matrix, or data frame"))
    }
    data <- data.frame(data, offsets)
    offsets <- tail(names(data), length(offsets))
  }
  model_terms <- terms(x, y, offsets = offsets)
  data[[deparse1(response(model_terms))]] <- y
  end <- length(data)
  data <- data[c(end, seq_len(end - 1))]

  ModelFrame(model_terms, data, na.rm = na.rm, names = rownames(data),
             weights = weights, strata = strata, ...)
}


ModelFrame.ModelFrame <- function(x, na.rm = TRUE, ...) {
  extras <- list(...)
  for (name in names(extras)) {
    x[[paste0("(", name, ")")]] <- extras[[name]]
  }
  if (na.rm) na.omit(x) else x
}


ModelFrame.ModelTerms <- function(x, data, ...) {
  ModelFrame(structure(
    as.data.frame(data), terms = x, class = c("ModelFrame", "data.frame")
  ), ...)
}


ModelFrame.recipe <- function(x, ...) {
  x <- prep(x)
  data <- bake(x, NULL)

  weights_name <- case_weights_name(x)
  weights <- if (length(weights_name)) data[[weights_name]]
  strata_name <- case_strata_name(x)
  strata <- if (length(strata_name)) data[[strata_name]]

  model_terms <- terms(x)
  data[[deparse1(response(model_terms))]] <- response(model_terms, data)
  data <- data[all.vars(model_formula(model_terms))]

  ModelFrame(model_terms, data, na.rm = FALSE,
             names = if (is.null(data[["(names)"]])) rownames(data),
             weights = weights, strata = strata)
}


#################### ModelFrame Formulas ####################


formula.ModelFrame <- function(x, ...) {
  model_formula(terms(x))
}


formula.ModelTerms <- function(x, ...) {
  formula(asS3(x))
}


inline_calls <- function(x) {
  if (is.call(x)) {
    call_name <- as.character(x[[1]])
    unique(c(call_name, unlist(map(inline_calls, x[-1]))))
  }
}


model_formula <- function(x) {
  x <- formula(x)
  response <- response(x)
  if (!is.null(response)) x[[2]] <- as.name(deparse1(response))
  x
}


#################### ModelFrame Terms ####################


terms.formula <- function(x, ...) {
  ModelFormulaTerms(structure(
    stats::terms.formula(x, ...),
    .Environment = asNamespace("MachineShop")
  ))
}


terms.list <- function(
  x, y = NULL, intercept = TRUE, all_numeric = FALSE, ...
) {
  if (is.character(y)) y <- as.name(y)
  has_y <- 1L - is.null(y)

  is_names <- map_logi(is.name, x)
  name_inds <- which(is_names)
  noname_inds <- which(!is_names)

  x_char <- character(length(x))
  x_char[name_inds] <- as.character(x[name_inds])
  x_char[noname_inds] <- map_chr(deparse, x[noname_inds])

  valid_calls <- map_logi(function(var) {
    is.call(var) && var[[1]] == "offset"
  }, x[noname_inds])
  if (!all(valid_calls)) {
    throw(Error("non-offset calls in variable specifications"))
  }
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
    x_char[name_inds] <- map_chr(as.character, x[name_inds])
    "ModelDesignTerms"
  } else {
    "ModelFormulaTerms"
  }

  var_names <- c(if (has_y) deparse1(y), x_char)
  label_names <- x_char[name_inds]

  if (class == "ModelDesignTerms") {
    factors <- cbind(as.integer(var_names %in% label_names))
    rownames(factors) <- var_names
  } else {
    factors <- matrix(0L, length(var_names), length(label_names),
                      dimnames = list(var_names, label_names))
    term_match <- match(var_names, label_names, nomatch = 0L)
    factors[cbind(var_names[term_match > 0], label_names[term_match])] <- 1L
  }

  new(class, structure(
    fo,
    variables = as.call(c(.(list), y, x)),
    offset = if (length(offsets)) offsets,
    factors = factors,
    term.labels = label_names,
    order = rep(1L, length(label_names)),
    intercept = as.integer(intercept),
    response = has_y,
    .Environment = asNamespace("MachineShop"),
    class = c("terms", "formula")
  ))
}


terms.matrix <- function(x, y = NULL, offsets = NULL, ...) {
  stopifnot(is.character(colnames(x)))
  stopifnot(!anyDuplicated(colnames(x)))

  labels <- colnames(x)
  response <- if (!is.null(y)) make.unique(c(labels, "y"))[length(labels) + 1]
  labels <- map(as.name, labels)

  if (length(offsets)) {
    offsets <- paste0("offset(", offsets, ")")
    labels <- c(labels, map(str2lang, offsets))
  }

  terms(labels, response, all_numeric = is.numeric(x))
}


terms.ModelFrame <- function(x, ...) {
  attr(x, "terms")
}


terms.ModelTerms <- function(x, ...) {
  x
}


terms.ModeledDesignTerms <- function(x, ...) {
  as(x, "ModelDesignTerms")
}


terms.ModeledFormulaTerms <- function(x, ...) {
  as(x, "ModelFormulaTerms")
}


terms.ModeledFrame <- function(x, ...) {
  ModeledInput(NextMethod(), model = x@model)
}


terms.recipe <- function(x, original = FALSE, ...) {
  terms.recipe_info(summary(x, original = original))
}


terms.recipe_info <- function(x, ...) {

  first <- function(x) head(x, 1)
  get_vars <- function(roles = NULL, types = NULL) {
    is_match <- by(x, x$variable, function(split) {
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

  num_roles <- sum(lengths(list(binom, surv, matrix, other)) > 0)
  if (num_roles > 1 || length(other) > 1) {
    throw(Error("specified outcome is not a single variable, binomial ",
                "variable with roles 'binom_x' and 'binom_size', survival ",
                "variables with roles 'surv_time' and 'surv_event', or ",
                "multiple numeric variables"))
  }

  outcome <- if (length(other)) {
    other
  } else if (!is.na(surv["time"])) {
    args <- as.name(surv["time"])
    if (!is.na(surv["event"])) args <- c(args, as.name(surv["event"]))
    as.call(c(.(Surv), args))
  } else if (length(surv)) {
    msg <- "survival outcome role 'surv_event' specified without 'surv_time'"
    throw(Error(msg))
  } else if (all(!is.na(binom[c("count", "size")]))) {
    call("BinomialVariate", as.name(binom["count"]), as.name(binom["size"]))
  } else if (length(binom)) {
    throw(Error("binomial outcome must have 'binom_x' and 'binom_size' roles"))
  } else if (length(matrix)) {
    as.call(c(.(cbind), map(as.name, matrix)))
  }

  is_predictor <- x$role == "predictor"
  predictors <- map(as.name, x$variable[is_predictor])
  all_numeric <- all(x$type[is_predictor] == "numeric")

  offsets <- get_vars("pred_offset", "numeric")
  if (length(offsets)) {
    offsets <- paste0("offset(", offsets, ")")
    predictors <- c(predictors, map(str2lang, offsets))
  }

  terms(predictors, outcome, all_numeric = all_numeric)

}


#################### ModelFrame Design Matrices ####################


model.matrix.ModelDesignTerms <- function(object, data, ...) {
  data <- data[labels(object)]
  assign <- seq_len(ncol(data))
  if (attr(object, "intercept")) {
    data <- cbind("(Intercept)" = 1, data)
    assign <- c(0, assign)
  }
  structure(as.matrix(data), assign = assign)
}


model.matrix.ModelFormulaTerms <- function(object, data, ...) {
  fo <- delete.response(object)
  mf <- model.frame(fo, data, na.action = na.pass)
  model.matrix.default(fo, mf, ...)
}


model.matrix.ModelFrame <- function(object, intercept = NULL, ...) {
  model_terms <- terms(object)
  if (!is.null(intercept)) {
    attr(model_terms, "intercept") <- as.integer(intercept)
  }
  model.matrix(model_terms, object, ...)
}


model.matrix.SelectedInput <- function(object, ...) {
  throw(Error("cannot create a design matrix from a ", class(object)))
}


model.offset <- function(x) {
  stopifnot(is(x, "ModelFrame"))
  keep <- 1 + c(0, attr(terms(x), "offset"))
  offsets <- eval(attr(terms(x), "variables")[keep], x)
  Reduce("+", offsets)
}
