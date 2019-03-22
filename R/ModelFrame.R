#' ModelFrame Class
#' 
#' Class for storing a data frame, formula, and optionally weights for fitting
#' MLModels.
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
ModelFrame.formula <- function(x, data, weights = NULL, strata = NULL,
                               na.rm = TRUE, ...) {
  data <- as.data.frame(data)
  
  modelterms <- terms(x, data = data)
  modelframe <- data[, all.vars(modelterms), drop = FALSE]
  
  weights <- eval(substitute(weights), data)
  if (!is.null(weights)) {
    stopifnot(length(weights) == nrow(data))
    modelframe[["(weights)"]] <- weights
  }
  
  strata <- eval(substitute(strata), data)
  if (!is.null(strata)) {
    stopifnot(length(strata) == nrow(data))
    modelframe[["(strata)"]] <- strata
  }
  
  na.action <- list(...)$na.action
  if (!is.null(na.action)) {
    depwarn("'na.action' argument to ModelFrame is deprecated",
            "use 'na.rm' instead")
    modelframe <- na.action(modelframe)
  } else if (na.rm) {
    modelframe <- na.omit(modelframe)
  }
  
  attr(modelframe, "terms") <- modelterms
  class(modelframe) <- c("ModelFrame", "data.frame")
  modelframe
}


#' @rdname ModelFrame-methods
#' 
#' @param y response variable.
#'
ModelFrame.matrix <- function(x, y, weights = NULL, strata = NULL,
                              na.rm = TRUE, ...) {
  data <- as.data.frame(x)
  end <- ncol(x) + 1
  y_name <- make.unique(c(names(data), "y"))[end]
  data[[y_name]] <- y
  data <- cbind(data[end], data[-end])
  
  do.call(ModelFrame, list(formula(data), data,
                           weights = weights, strata = strata,
                           na.rm = na.rm, ...))
}


ModelFrame.ModelFrame <- function(x, na.rm = TRUE, ...) {
  do.call(ModelFrame, list(formula(terms(x)), x,
                           weights = x[["(weights)"]], strata = x[["(strata)"]],
                           na.rm = na.rm, ...))
}


ModelFrame.recipe <- function(x, ...) {
  x <- prep(x, retain = TRUE)
  df <- juice(x)
  
  info <- summary(x)
  
  var_name <- info$variable[info$role == "case_weight"]
  weights <- if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) df[[var_name]] else
      stop("multiple case weights specified")

  var_name <- info$variable[info$role == "case_strata"]
  strata <- if (length(var_name) == 0) NULL else
    if (length(var_name) == 1) df[[var_name]] else
      stop("multiple strata variables specified")

  do.call(ModelFrame, list(formula(terms(x)), df,
                           weights = weights, strata = strata,
                           na.rm = FALSE))
}
