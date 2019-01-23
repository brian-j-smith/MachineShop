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
#' @seealso \code{\link{formula}}, \code{\link{na.fail}}, \code{\link{na.omit}},
#' \code{\link{na.pass}}
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
#' @param na.action action to take if cases contain missing values.  The default
#' is first any \code{na.action} attribute of \code{data}, second a
#' \code{na.action} setting of \code{\link{options}}, and third
#' \code{\link{na.fail}} if unset.
#' @param ... arguments passed to other methods.
#' 
ModelFrame.formula <- function(x, data, weights = NULL, strata = NULL,
                               na.action = NULL, ...) {
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
  
  if (is.null(na.action)) na.action <- na.action(data)
  if (is.null(na.action)) na.action <- get(getOption("na.action"))
  if (is.null(na.action)) na.action <- na.fail
  modelframe <- na.action(modelframe)
  
  attr(modelframe, "terms") <- modelterms
  class(modelframe) <- c("ModelFrame", "data.frame")
  modelframe
}


#' @rdname ModelFrame-methods
#' 
#' @param y response variable.
#'
ModelFrame.matrix <- function(x, y, weights = NULL, strata = NULL,
                              na.action = NULL, ...) {
  data <- as.data.frame(x)
  end <- ncol(x) + 1
  y_name <- make.unique(c(names(data), "y"))[end]
  data[[y_name]] <- y
  data <- cbind(data[end], data[-end])
  
  do.call(ModelFrame, list(formula(data), data,
                           weights = weights, strata = strata,
                           na.action = na.action))
}


ModelFrame.ModelFrame <- function(x, na.action = NULL, ...) {
  do.call(ModelFrame, list(formula(terms(x)), x,
                           weights = x[["(weights)"]], strata = x[["(strata)"]],
                           na.action = na.action))
}


ModelFrame.recipe <- function(x, na.action = NULL, ...) {
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

  do.call(ModelFrame, list(formula(x), df,
                           weights = weights, strata = strata,
                           na.action = na.action))
}
