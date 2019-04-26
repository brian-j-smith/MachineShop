#' ModelFrame Class
#' 
#' Class for storing data, formulas, and other attributes for fitting MLModels.
#' 
#' @name ModelFrame
#' @rdname ModelFrame-methods
#' 
#' @param x model \code{\link{formula}} or \code{matrix} of predictor variables.
#' 
#' @return \code{ModelFrame} class object that inherits from \code{data.frame}.
#' 
#' @seealso \code{\link{formula}}
#' 
#' @examples
#' mf <- ModelFrame(ncases / (ncases + ncontrols) ~ agegp + tobgp + alcgp,
#'                  data = esoph, weights = with(esoph, ncases + ncontrols))
#' gbmfit <- fit(mf, model = GBMModel)
#' varimp(gbmfit)
#' 
ModelFrame <- function(x, ...) {
  UseMethod("ModelFrame")
}


#' @rdname ModelFrame-methods
#'
#' @param data \code{data.frame} or an object that can be converted to one.
#' @param na.rm logical indicating whether to remove cases with \code{NA} values
#' @param weights vector of case weights.
#' @param strata vector of stratification levels.
#' for any of the model variables.
#' @param ... arguments passed to other methods.
#' 
ModelFrame.formula <- function(x, data, na.rm = TRUE, weights = NULL,
                               strata = NULL, ...) {
  invalid_calls <- setdiff(inline_calls(predictors(x)), valid_predictor_calls)
  if (length(invalid_calls)) {
    stop(
      "unsupported predictor variable functions: ", toString(invalid_calls),
      "; use a recipe or include transformed predictors in the data frame"
    )
  }
  
  data <- as.data.frame(data)
  model_terms <- structure(
    terms(x, data = data),
    class = c("FormulaTerms", "terms", "formula")
  )
  data[deparse(response(model_terms))] <- response(model_terms, data)
  
  ModelFrame(model_terms, data, na.rm = na.rm,
             weights = weights, strata = strata, ...)
}


#' @rdname ModelFrame-methods
#' 
#' @param y response variable.
#'
ModelFrame.matrix <- function(x, y = NULL, na.rm = TRUE,
                              weights = NULL, strata = NULL, ...) {
  data <- as.data.frame(x)
  colnames(x) <- names(data)
  model_terms <- terms(x, y)
  data[deparse(response(model_terms))] <- y
  
  ModelFrame(model_terms, data, na.rm = na.rm,
             weights = weights, strata = strata, ...)
}


ModelFrame.ModelFrame <- function(x, na.rm = TRUE, na.action = NULL, ...) {
  vars <- as.data.frame(do.call(cbind, list(...)))
  names(vars) <- sapply(names(vars), function(x) paste0("(", x, ")"))
  x[names(vars)] <- vars

  if (!is.null(na.action)) {
    depwarn("'na.action' argument to ModelFrame is deprecated",
            "use 'na.rm' instead", expired = Sys.Date() > "2019-06-01")
    na.action(x)
  } else if (na.rm) {
    na.omit(x)
  } else x
}


ModelFrame.recipe <- function(x, ...) {
  x <- prep(x)
  data <- juice(x)
  
  info <- summary(x)
  
  var_name <- info$variable[info$role == "case_weight"]
  weights <- if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) data[[var_name]] else
      stop("multiple case weights specified")

  var_name <- info$variable[info$role == "case_strata"]
  strata <- if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) data[[var_name]] else
      stop("multiple strata variables specified")
  
  model_terms <- terms(x)
  data[deparse(response(model_terms))] <- response(model_terms, data)

  ModelFrame(model_terms, data, na.rm = FALSE,
             weights = weights, strata = strata)
}


ModelFrame.terms <- function(x, data, ...) {
  data[all.vars(model_formula(x))] %>%
    structure(terms = x, class = c("ModelFrame", class(data))) %>%
    ModelFrame(...)
}


#################### ModelFrame Conversion ####################


as.data.frame.ModelFrame <- function(x, ...) {
  structure(x, terms = NULL, class = "data.frame")
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
  if (!is.null(response)) x[[2]] <- as.symbol(deparse(response))
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
  "%in%", "(", ".", ":", "I"
)


#################### ModelFrame Terms ####################


terms.character <- function(x, y = NULL, intercept = TRUE, all_numeric = FALSE,
                            ...) {
  x_names <- x
  x <- lapply(x, as.symbol)
  
  if (is.character(y)) y <- as.symbol(y)
  y_indicator <- 1L - is.null(y)
  
  fo <- parse(text = paste(
    "y ~",
    if (length(x)) {
      paste(sapply(x, deparse, backtick = TRUE), collapse = "+")
    } else if (intercept) "1",
    if (!intercept) "- 1"
  ))[[1]]
  fo[[2]] <- y
  
  var_names <- c(if (y_indicator) deparse(y), x_names)
  
  if (all_numeric) {
    class <- "DesignTerms"
    factors <- cbind(rep(c(0L, 1L), c(y_indicator, length(x))))
    rownames(factors) <- var_names
  } else {
    class <- "FormulaTerms"
    factors <- rbind(rep(0L, y_indicator * length(x)), diag(1L, length(x)))
    dimnames(factors) <- list(var_names, x_names)
  }
  
  structure(
    fo,
    variables = as.call(c(quote(list), y, x)),
    factors = factors,
    term.labels = x_names,
    order = rep(1L, length(x)),
    intercept = as.integer(intercept),
    response = y_indicator,
    .Environment = parent.frame(),
    class = c(class, "terms", "formula")
  )
}


terms.matrix <- function(x, y = NULL, ...) {
  stopifnot(is.character(colnames(x)))
  stopifnot(!anyDuplicated(colnames(x)))
  
  labels <- colnames(x)
  response <- if (!is.null(y)) make.unique(c(labels, "y"))[length(labels) + 1]

  terms(labels, response, all_numeric = is.numeric(x))
}


terms.ModelFrame <- function(x, ...) {
  attr(x, "terms")
}


terms.recipe <- function(x, original = FALSE, ...) {
  info <- summary(x, original = original)
  
  get_vars <- function(roles = NULL, types = NULL) {
    is_match <- by(info, info$variable, function(split) {
      all(roles %in% split$role) && all(types %in% split$type)
    })
    names(is_match)[is_match]
  }
  
  outcome <- NULL
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

  is_predictor <- info$role == "predictor"
  predictors <- info$variable[is_predictor]
  all_numeric <- all(info$type[is_predictor] == "numeric")
  
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


#################### ModelFrame Preprocessing ####################


preprocess <- function(x, newdata = NULL) {
  ModelFrame(delete.response(terms(x)), predictors(x, newdata), na.rm = FALSE)
}
