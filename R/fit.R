#' Model Fitting
#'
#' Fit a model to estimate its parameters from a data set.
#'
#' @name fit
#' @rdname fit-methods
#'
#' @param x \link[=inputs]{input} specifying a relationship between model
#'   predictor and response variables.  Alternatively, a \link[=models]{model}
#'   function or call may be given first followed by the input specification.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or call; ignored
#'   and can be omitted when fitting \link[=ModeledInput]{modeled inputs}.
#' @param ... arguments passed to other methods.
#'
#' @return \code{MLModelFit} class object.
#'
#' @seealso \code{\link{as.MLModel}}, \code{\link{response}},
#' \code{\link{predict}}, \code{\link{varimp}}
#'
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#'
#' gbm_fit <- fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'                data = Melanoma, model = GBMModel)
#' varimp(gbm_fit)
#'
fit <- function(x, ...) {
  UseMethod("fit")
}


#' @rdname fit-methods
#'
fit.formula <- function(x, data, model, ...) {
  fit(ModelFrame(x, data, na.rm = FALSE), model)
}


#' @rdname fit-methods
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
  model <- if (missing(model)) NullModel else getMLObject(model, "MLModel")
  .fit(x, model)
}


#' @rdname fit-methods
#'
#' @details
#' Variables in \code{recipe} specifications may be designated as case weights
#' with the \code{\link{role_case}} function.
#'
fit.recipe <- function(x, model, ...) {
  model <- if (missing(model)) NullModel else getMLObject(model, "MLModel")
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
  mf <- ModelFrame(inputs, na.rm = FALSE)
  if (is.null(mf[["(weights)"]])) mf[["(weights)"]] <- 1

  y <- response(mf)
  if (!any(map_logi(function(type) is_response(y, type), x@response_types))) {
    stop("invalid response type '", class(y)[1], "' for ", x@name)
  }

  requireModelNamespaces(x@packages)

  params_env <- list2env(list(
    formula = formula(mf),
    data = mf,
    weights = model.weights(mf),
    y = y,
    nobs = nrow(mf),
    nvars = nvars(mf, x)
  ), parent = new.env(parent = asNamespace("MachineShop")))
  environment(params_env$formula) <- params_env

  args <- c(mget(c("formula", "data", "weights"), params_env), x@params)

  do.call(x@fit, args, envir = params_env) %>%
    MLModelFit(paste0(x@name, "Fit"), model = x, x = inputs, y = y)
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
  .fit(model, prep(ModelRecipe(x)))
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
