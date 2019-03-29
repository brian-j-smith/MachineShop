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
