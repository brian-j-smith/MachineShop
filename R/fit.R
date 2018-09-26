#' Model Fitting
#' 
#' Fit a model to estimate its parameters from a data set.
#' 
#' @name fit
#' @rdname fit-methods
#' 
#' @param x defined relationship between model predictors and an outcome.  May
#' be a model.frame (data.frame) containing a formula, data, and optionally case
#' weights; a formula; or a recipe.
#' @param ... arguments passed to other methods.
#' 
fit <- function(x, ...) {
  UseMethod("fit", x)
}


#' @rdname fit-methods
#' 
fit.data.frame <- function(x, model, ...) {
  model <- getMLObject(model, "MLModel")
  
  y <- response(x)
  if (!any(sapply(model@types, function(type) is(y, type)))) {
    stop("invalid response type '", class(y)[1], "' for ", model@name)
  }
  
  requireModelNamespaces(model@packages)
  fo <- formula(terms(x))
  fo[[2]] <- formula(x)[[2]]
  weights <- model.weights(x)
  if (is.null(weights)) weights <- rep(1, nrow(x))
  args <- c(list(formula = fo, data = x, weights = weights), model@params)
  data_params <- list(nobs = nrow(x),
                      nvars = model@nvars(x[1, , drop = FALSE]),
                      y = y)
  do.call(model@fit, args, envir = list2env(c(args, data_params))) %>%
    asMLModelFit(paste0(model@name, "Fit"), model)
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
#' ## Survival analysis example
#' library(survival)
#' 
#' gbmfit <- fit(Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
#'                                    pat.karno + meal.cal + wt.loss,
#'               data = lung,
#'               GBMModel)
#' (vi <- varimp(gbmfit))
#' plot(vi)
#' 
fit.formula <- function(x, data, model, ...) {
  fit(model.frame(x, data, na.action = NULL), model)
}


#' @rdname fit-methods
#' 
fit.recipe <- function(x, model, ...) {
  x <- prep(x, retain = TRUE)
  fit(formula(x), juice(x), model)
}
