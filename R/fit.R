#' Model Fitting
#' 
#' Fit a model to estimate its parameters from a data set.
#' 
#' @name fit
#' @rdname fit-methods
#' 
#' @param x defined relationship between model predictors and an outcome.  May
#' be a ModelFrame containing a formula, data, and optionally case weights; a
#' formula; or a recipe.
#' @param ... arguments passed to other methods.
#' 
fit <- function(x, ...) {
  UseMethod("fit")
}


#' @rdname fit-methods
#' 
#' @param data data frame containing observed predictors and outcomes.
#' @param model MLModel object, constructor function, or character string
#' naming a constructor function that returns an MLModel object.
#' 
#' @return MLModelFit class object.
#' 
#' @seealso  \code{\link{tune}}, \code{\link[stats]{model.frame}},
#' \code{\link[recipes]{recipe}}, \code{\link{predict}}, \code{\link{varimp}}
#' 
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' gbmfit <- fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'               data = Melanoma, model = GBMModel)
#' varimp(gbmfit)
#' 
fit.formula <- function(x, data, model, ...) {
  fit(ModelFrame(x, data, na.action = na.pass), model)
}


#' @rdname fit-methods
#' 
fit.ModelFrame <- function(x, model, ...) {
  .fit(getMLObject(model, "MLModel"), x)
}


#' @rdname fit-methods
#' 
fit.recipe <- function(x, model, ...) {
  .fit(getMLObject(model, "MLModel"), x)
}


.fit <- function(model, x, ...) {
  UseMethod(".fit")
}


.fit.MLModel <- function(model, x, ...) {
  mf <- ModelFrame(x, na.action = na.pass)
  
  y <- response(mf)
  if (!any(sapply(model@types, function(type) is_response(y, type)))) {
    stop("invalid response type '", class(y)[1], "' for ", model@name)
  }
  
  requireModelNamespaces(model@packages)
  fo <- formula(terms(mf))
  weights <- model.weights(mf)
  if (is.null(weights)) weights <- rep(1, nrow(mf))
  args <- c(list(formula = fo, data = mf, weights = weights), model@params)
  data_params <- list(y = y, nobs = nrow(mf))
  data_params$nvars <-  model@nvars(mf[1, , drop = FALSE])
  
  do.call(model@fit, args, envir = list2env(c(args, data_params))) %>%
    asMLModelFit(paste0(model@name, "Fit"), model, x, y)
}
