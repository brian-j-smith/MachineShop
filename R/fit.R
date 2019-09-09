#' Model Fitting
#' 
#' Fit a model to estimate its parameters from a data set.
#' 
#' @name fit
#' @rdname fit-methods
#' 
#' @param x defines a relationship between model predictor and response
#' variables.  May be a \code{\link{formula}}, design \code{\link{matrix}} of
#' predictors, \code{\link{ModelFrame}}, or untrained
#' \code{\link[recipes]{recipe}}.
#' @param ... arguments passed to other methods.
#' 
fit <- function(x, ...) {
  UseMethod("fit")
}


#' @rdname fit-methods
#' 
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#' outcomes.
#' @param model \link[=models]{model} function, function name, or call.
#' 
#' @return \code{MLModelFit} class object.
#' 
#' @seealso \code{\link{response}}, \code{\link{predict}}, \code{\link{varimp}}
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
  fit(ModelFrame(x, data, na.rm = FALSE), model)
}


#' @rdname fit-methods
#' 
#' @param y response variable.
#' 
fit.matrix <- function(x, y, model, ...) {
  fit(ModelFrame(x, y, na.rm = FALSE), model)
}


#' @rdname fit-methods
#' 
#' @details
#' User-specified case weights may be specified for \code{ModelFrames} upon
#' creation with the \code{\link[=ModelFrame]{weights}} argument in its
#' constructor.
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
  .fit(getMLObject(model, "MLModel"), prep(ModelRecipe(x)))
}


.fit <- function(model, x, ...) {
  UseMethod(".fit")
}


.fit.MLModel <- function(model, x, ...) {
  mf <- ModelFrame(x, na.rm = FALSE)
  if (is.null(mf[["(weights)"]])) mf[["(weights)"]] <- 1
  
  y <- response(mf)
  if (!any(sapply(model@response_types, function(type) is_response(y, type)))) {
    stop("invalid response type '", class(y)[1], "' for ", model@name)
  }
  
  requireModelNamespaces(model@packages)
  
  envir <- list2env(within(list(), {
    formula <- formula(mf)
    data <- mf
    weights <- model.weights(mf)
    y <- y
    nobs <- nrow(mf)
    nvars <- nvars(mf, model)
  }), parent = globalenv())
  environment(envir$formula) <- envir
  
  args <- c(mget(c("formula", "data", "weights"), envir), model@params)
  
  do.call(model@fit, args, envir = envir) %>%
    asMLModelFit(paste0(model@name, "Fit"), model, x, y)
}


eval_fit <- function(data, formula, matrix) {
  use_model_matrix <- if (missing(formula)) TRUE else
    if (missing(matrix)) FALSE else
      is(terms(data), "DesignTerms")
  
  if (use_model_matrix) {
    envir <- list(
      x = model.matrix(data, intercept = FALSE),
      y = response(data)
    )
    eval(substitute(matrix), envir, parent.frame())
  } else {
    eval.parent(substitute(formula))
  }
}
