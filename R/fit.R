#' Model Fitting
#' 
#' Fit a model to estimate its parameters from a data set.
#' 
#' @name fit
#' @rdname fit-methods
#' 
#' @param x defined relationship between model predictors and an outcome.  May
#' be a \code{ModelFrame} containing a formula, data, and optionally case
#' weights; a \code{formula}; or a \code{recipe}.
#' @param ... arguments passed to other methods.
#' 
fit <- function(x, ...) {
  UseMethod("fit")
}


#' @rdname fit-methods
#' 
#' @param data \code{data.frame} containing observed predictors and outcomes.
#' @param model \code{MLModel} object, constructor function, or character string
#' naming a constructor function that returns an \code{MLModel} object.
#' 
#' @return \code{MLModelFit} class object.
#' 
#' @seealso \code{\link{ModelFrame}}, \code{\link[recipes]{recipe}},
#' \code{\link{modelinfo}}, \code{\link{tune}}, \code{\link{predict}},
#' \code{\link{varimp}}
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
#' @details
#' User-specified case weights may be specified for
#' \code{\link[MachineShop:ModelFrame-methods]{ModelFrames}} upon
#' creation with the \code{weights} argument in its constructor.
#' 
fit.ModelFrame <- function(x, model, ...) {
  .fit(getMLObject(model, "MLModel"), x)
}


#' @rdname fit-methods
#' 
#' @details
#' Variables in a \code{recipe} may be used as case weights by defining a
#' "case_weight" \code{\link[recipes:roles]{role}} for them.
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
  
  envir <- list2env(within(list(), {
    formula <- formula(terms(mf))
    data <- as.data.frame(mf)
    weights <- model.weights(mf)
    if (is.null(weights)) weights <- rep(1, nrow(mf))
    y <- y
    nobs <- nrow(mf)
    nvars <- nvars(mf, model)
  }), parent = globalenv())
  environment(envir$formula) <- envir
  
  args <- c(mget(c("formula", "data", "weights"), envir), model@params)
  
  do.call(model@fit, args, envir = envir) %>%
    asMLModelFit(paste0(model@name, "Fit"), model, x, y)
}
