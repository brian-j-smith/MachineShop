#' Model Fitting
#' 
#' Fit a model to estimate its parameters from a data set.
#' 
#' @name fit
#' @rdname fit-methods
#' 
#' @param object prediction model object.
#' @param x defined relationship between model predictors and an outcome.  May
#' be a model.frame (data.frame) containing a formula, data, and optionally case
#' weights; a formula; or a recipe.
#' @param ... arguments passed to other methods.
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
#' gbmfit <- fit(GBMModel(),
#'               Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
#'                                    pat.karno + meal.cal + wt.loss,
#'               data = lung)
#' (vi <- varimp(gbmfit))
#' plot(vi)
#' 
setGeneric("fit", function(object, x, ...) standardGeneric("fit"))


#' @rdname fit-methods
#' @aliases fit,MLModel,data.frame-method
#' 
setMethod("fit", c("MLModel", "data.frame"),
  function(object, x, ...) {
    requireModelNamespaces(object@packages)
    fo <- formula(terms(x))
    fo[[2]] <- formula(x)[[2]]
    weights <- model.weights(x)
    if(is.null(weights)) weights <- rep(1, nrow(x))
    params <- lapply(object@params, eval,
                     list(formula = fo, data = x, weights = weights))
    do.call(object@fit, c(list(fo, x, weights), params))
  }
)


#' @rdname fit-methods
#' @aliases fit,MLModel,formula-method
#' 
#' @param data data frame containing observed predictors and outcomes.
#' 
setMethod("fit", c("MLModel", "formula"),
  function(object, x, data, ...) {
    fit(object, model.frame(x, data, na.action = NULL))
  }
)


#' @rdname fit-methods
#' @aliases fit,MLModel,recipe-method
#' 
setMethod("fit", c("MLModel", "recipe"),
  function(object, x, ...) {
    x <- prep(x, retain = TRUE)
    fit(object, formula(x), juice(x))
  }
)
