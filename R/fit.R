#' Model Fitting
#'
#' Fit a model to estimate its parameters from a data set.
#'
#' @name fit
#' @rdname fit-methods
#'
#' @param ... arguments passed from the generic function to its methods and from
#'   the \code{MLModel} and \code{MLModelFunction} methods to others.  The
#'   first argument of each \code{fit} method is positional and, as such, must
#'   be given first in calls to them.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param input \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param object model \link[=ModelSpecification]{specification}.
#' @param model \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.  A model
#'   can be given first followed by any of the variable specifications, and the
#'   argument can be omitted altogether in the case of
#'   \link[=ModeledInput]{modeled inputs}.
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
fit <- function(...) {
  UseMethod("fit")
}


#' @rdname fit-methods
#'
fit.formula <- function(formula, data, model, ...) {
  fit(ModelSpecification(formula, data, model = model, control = NULL), ...)
}


#' @rdname fit-methods
#'
fit.matrix <- function(x, y, model, ...) {
  fit(ModelSpecification(x, y, model = model, control = NULL), ...)
}


#' @rdname fit-methods
#'
#' @details
#' User-specified case weights may be specified for \code{ModelFrames} upon
#' creation with the \code{\link[=ModelFrame]{weights}} argument in its
#' constructor.
#'
fit.ModelFrame <- function(input, model = NULL, ...) {
  fit(ModelSpecification(input, model = model, control = NULL), ...)
}


#' @rdname fit-methods
#'
#' @details
#' Variables in \code{recipe} specifications may be designated as case weights
#' with the \code{\link{role_case}} function.
#'
fit.recipe <- function(input, model = NULL, ...) {
  fit(ModelSpecification(input, model = model, control = NULL), ...)
}


#' @rdname fit-methods
#'
fit.ModelSpecification <- function(object, ...) {
  if (is_optim_method(object)) {
    .fit_optim(object)
  } else {
    .fit(as.MLInput(object), model = as.MLModel(object))
  }
}


#' @rdname fit-methods
#'
fit.MLModel <- function(model, ...) {
  fit(..., model = model)
}


#' @rdname fit-methods
#'
fit.MLModelFunction <- function(model, ...) {
  fit(as.MLModel(model), ...)
}


.fit <- function(object, ...) {
  UseMethod(".fit")
}


.fit.MLInput <- function(object, model, ...) {
  .fit(model, input = object)
}


.fit.MLModel <- function(object, input, ...) {
  input_prep <- prep(input)
  mf <- ModelFrame(input_prep, na.rm = FALSE)
  y <- response(mf)

  info <- data.frame(type = object@response_types, weights = object@weights)
  is_response_types <- is_response(y, info$type)
  if (!any(is_response_types)) {
    throw(TypeError(y, info$type, paste(object@name, "response variable")))
  }
  weights <- case_weights(mf)
  if (!all(info$weights[is_response_types]) || is.null(weights)) {
    throw(check_equal_weights(weights))
    mf <- ModelFrame(mf, weights = 1, na.rm = FALSE)
  }

  throw(check_packages(object@packages))

  params_env <- list2env(list(
    formula = formula(mf),
    data = mf,
    weights = case_weights(mf),
    y = y,
    nobs = nrow(mf),
    nvars = nvars(mf, object)
  ), parent = new.env(parent = asNamespace("MachineShop")))
  environment(params_env$formula) <- params_env

  args <- c(mget(c("formula", "data", "weights"), params_env), object@params)

  do.call(object@fit, args, envir = params_env) %>%
    MLModelFit(paste0(object@name, "Fit"), input = input_prep, model = object)
}


.fit_optim <- function(object, ...) {
  mloptim <- get_optim_field(object)
  throw(check_packages(mloptim@packages))
  tryCatch(
    optim(mloptim@fun, object, ...),
    error = function(cond) {
      msg <- conditionMessage(cond)
      if (is(mloptim, "SequentialOptimization")) {
        object <- set_optim_grid(
          object, random = mloptim@random, progress = mloptim@monitor$progress
        )
        throw(LocalWarning(
          mloptim@label, " failed: ", msg, "\n",
          "Performing a ", tolower(get_optim_field(object, "label")),
          " instead."
        ))
        fit(object, ...)
      } else {
        throw(Error(msg), call = sys.call(-4))
      }
    }
  )
}


eval_fit <- function(data = NULL, formula, matrix) {
  expr <- if (is(data, "ModelFrame") && is(terms(data), "ModelDesignTerms")) {
    bquote({
      x <- model.matrix(data, intercept = FALSE)
      y <- response(data)
      .(substitute(matrix))
    })
  } else {
    bquote({
      data <- as.data.frame(formula, data)
      .(substitute(formula))
    })
  }
  eval.parent(expr)
}
