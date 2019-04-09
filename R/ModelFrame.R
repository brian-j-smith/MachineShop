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
#'                  data = esoph, weights = ncases + ncontrols)
#' gbmfit <- fit(mf, model = GBMModel)
#' varimp(gbmfit)
#' 
ModelFrame <- function(x, ...) {
  UseMethod("ModelFrame")
}


#' @rdname ModelFrame-methods
#'
#' @param data \code{data.frame} or an object that can be converted to one.
#' @param weights vector of case weights.
#' @param strata vector of stratification levels.
#' @param na.rm logical indicating whether to remove cases with \code{NA} values
#' for any of the model variables.
#' @param ... arguments passed to other methods.
#' 
ModelFrame.formula <- function(x, data, na.rm = TRUE, weights = NULL,
                               strata = NULL, ...) {
  data <- as.data.frame(data)
  model_terms <- terms(x, data = data)
  class(model_terms) <- c("FormulaTerms", class(model_terms))
  
  ModelFrame(model_terms, data, na.rm = na.rm,
             weights = eval(substitute(weights), data),
             strata = eval(substitute(strata), data), ...)
}


#' @rdname ModelFrame-methods
#' 
#' @param y response variable.
#' @param intercept logical indicating whether to include an intercept in the
#' model formula.
#'
ModelFrame.matrix <- function(x, y = NULL, na.rm = TRUE, intercept = TRUE,
                              weights = NULL, strata = NULL, ...) {
  data <- as.data.frame(x)
  colnames(x) <- names(data)
  model_terms <- eval(substitute(terms(x, y, intercept = intercept)))
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
            "use 'na.rm' instead")
    na.action(x)
  } else if (na.rm) {
    na.omit(x)
  } else x
}


ModelFrame.recipe <- function(x, ...) {
  x <- prep(x, retain = TRUE)
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

  ModelFrame(terms(x), data, na.rm = FALSE, weights = weights, strata = strata)
}


ModelFrame.terms <- function(x, data, ...) {
  data[, all.vars(x), drop = FALSE] %>%
    structure(terms = x, class = c("ModelFrame", class(data))) %>%
    ModelFrame(...)
}


#################### ModelFrame Terms ####################


terms.character <- function(labels, response = NULL, intercept = TRUE) {
  if (is.character(response)) response <- as.symbol(response)
  response_indicator <- 1L - is.null(response)
  
  fo <- parse(text = paste(
    "response ~",
    paste(if (length(labels)) labels else "1", collapse = "+"),
    if (!intercept) "- 1"
  ))[[1]]
  fo[[2]] <- response
  
  all_vars <- c(if (response_indicator) deparse(response), labels)
  
  structure(
    fo,
    variables = as.call(c(quote(list), response, lapply(labels, as.symbol))),
    factors = matrix(NA_integer_, nrow = length(all_vars), ncol = 0,
                     dimnames = list(all_vars, NULL)),
    term.labels = labels,
    order = rep(1L, length(labels)),
    intercept = as.integer(intercept),
    response = response_indicator,
    .Environment = parent.frame(),
    class = c("DesignTerms", "terms", "formula")
  )
}


terms.matrix <- function(x, y = NULL, intercept = TRUE) {
  stopifnot(is.character(colnames(x)))
  stopifnot(!anyDuplicated(colnames(x)))
  
  labels <- colnames(x)
  response <- if (!is.null(y)) {
    make.unique(c(labels, deparse(substitute(y))))[length(labels) + 1]
  }
  
  terms(labels, response, intercept = intercept)
}


terms.ModelFrame <- function(x, ...) {
  attr(x, "terms")
}


terms.recipe <- function(x, ...) {
  info <- summary(x)
  
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

  predictors <- info$variable[info$role == "predictor"]
  
  terms(predictors, outcome)
}


#################### ModelFrame Design Matrices ####################


model.matrix.DesignTerms <- function(object, data, ...) {
  data <- data[, labels(object), drop = FALSE]
  assign <- seq_len(ncol(data))
  if (attr(object, "intercept")) {
    data <- cbind("(Intercept)" = 1, data)
    assign <- c(0, assign)
  }
  structure(as.matrix(data), assign = assign)
}


model.matrix.FormulaTerms <- function(object, data, ...) {
  model.matrix.default(object, as.data.frame(data), ...)
}


model.matrix.ModelFrame <- function(object, intercept = NULL, ...) {
  model_terms <- terms(object)
  if (!is.null(intercept)) {
    attr(model_terms, "intercept") <- as.integer(intercept)
  }
  model.matrix(model_terms, object, ...)
}


#################### ModelFrame Preprocessing ####################


preprocess <- function(x, data = NULL, ...) {
  UseMethod("preprocess")
}


preprocess.ModelFrame <- function(x, data = NULL, ...) {
  mf <- switch_class(
    data,
    "NULL" = x,
    "ModelFrame" = data,
    "data.frame" = ModelFrame(delete.response(terms(x)), data, na.rm = FALSE),
    "matrix" = ModelFrame(data, na.rm = FALSE)
  )
  if (is.null(mf)) stop("unsupported data structure") else mf
}


preprocess.recipe <- function(x, data = NULL, ...) {
  df <- if (is.null(data)) juice(x) else bake(x, data)
  ModelFrame(delete.response(terms(x)), df, na.rm = FALSE)
}
