#' Model Fitting
#'
#' Fit a model to estimate its parameters from a data set.
#'
#' @name fit
#' @rdname fit-methods
#'
#' @param x \link[=inputs]{input} specifying a relationship between model
#'   predictor and response variables.  Alternatively, a \link[=models]{model}
#'   function or object may be given first followed by the input specification.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or object;
#'   ignored and can be omitted when fitting
#'   \link[=ModeledInput]{modeled inputs}.
#' @param ... arguments passed to other methods.
#'
#' @return \code{MLModelFit} class object.
#'
#' @seealso \code{\link{as.MLModel}}, \code{\link{response}},
#' \code{\link{predict}}, \code{\link{varimp}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Survival response example
#' library(survival)
#'
#' gbm_fit <- fit(Surv(time, status) ~ ., data = veteran, model = GBMModel)
#' varimp(gbm_fit)
#' }
#'
fit <- function(x, ...) {
  UseMethod("fit")
}


#' @rdname fit-methods
#'
fit.formula <- function(x, data, model, ...) {
  mf <- do.call(ModelFrame, list(x, data, strata = response(x), na.rm = FALSE))
  fit(mf, model)
}


#' @rdname fit-methods
#'
fit.matrix <- function(x, y, model, ...) {
  mf <- ModelFrame(x, y, strata = y, na.rm = FALSE)
  fit(mf, model)
}


#' @rdname fit-methods
#'
#' @details
#' User-specified case weights may be specified for \code{ModelFrames} upon
#' creation with the \code{\link[=ModelFrame]{weights}} argument in its
#' constructor.
#'
fit.ModelFrame <- function(x, model, ...) {
  model <- if (missing(model)) NullModel() else get_MLModel(model)
  .fit(x, model)
}


#' @rdname fit-methods
#'
#' @details
#' Variables in \code{recipe} specifications may be designated as case weights
#' with the \code{\link{role_case}} function.
#'
fit.recipe <- function(x, model, ...) {
  model <- if (missing(model)) NullModel() else get_MLModel(model)
  .fit(x, model)
}


#' @rdname fit-methods
#'
fit.MLModel <- function(x, ...) {
  fit(..., model = x)
}


#' @rdname fit-methods
#'
fit.MLModelFunction <- function(x, ...) {
  fit(x(), ...)
}


.fit <- function(x, ...) {
  UseMethod(".fit")
}


.fit.MLModel <- function(x, inputs, ...) {
  inputs_prep <- prep(inputs)
  mf <- ModelFrame(inputs_prep, na.rm = FALSE)
  y <- response(mf)

  info <- data.frame(type = x@response_types, weights = x@weights)
  is_response_types <- is_response(y, info$type)
  if (!any(is_response_types)) {
    throw(TypeError(y, info$type, paste(x@name, "response variable")))
  }
  weights <- case_weights(mf)
  if (!all(info$weights[is_response_types]) || is.null(weights)) {
    throw(check_equal_weights(weights))
    mf <- ModelFrame(mf, weights = 1, na.rm = FALSE)
  }

  throw(check_packages(x@packages))

  params_env <- list2env(list(
    formula = formula(mf),
    data = mf,
    weights = case_weights(mf),
    y = y,
    nobs = nrow(mf),
    nvars = nvars(mf, x)
  ), parent = new.env(parent = asNamespace("MachineShop")))
  environment(params_env$formula) <- params_env

  args <- c(mget(c("formula", "data", "weights"), params_env), x@params)

  do.call(x@fit, args, envir = params_env) %>%
    MLModelFit(paste0(x@name, "Fit"), model = x, x = inputs_prep)
}


.fit.ModelFrame <- function(x, model, ...) {
  .fit(model, x)
}


.fit.ModeledFrame <- function(x, ...) {
  fit(as(x, "ModelFrame"), model = x@model)
}


.fit.ModeledRecipe <- function(x, ...) {
  fit(as(x, "ModelRecipe"), model = x@model)
}


.fit.recipe <- function(x, model, ...) {
  .fit(model, ModelRecipe(x))
}


.fit.TunedModeledRecipe <- function(x, ...) {
  fit(as(x, "TunedModelRecipe"), model = x@model)
}


eval_fit <- function(data, formula, matrix) {
  use_model_matrix <- if (missing(formula)) TRUE else
    if (missing(matrix)) FALSE else
      is(terms(data), "ModelDesignTerms")

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
