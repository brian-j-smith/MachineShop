#' Generalized Linear Model
#' 
#' Fits generalized linear models, specified by giving a symbolic description of
#' the linear predictor and a description of the error distribution.
#' 
#' @param family description of the error distribution and link function to be
#' used in the model.  Set automatically according to the class type of the
#' response variable.
#' @param ... arguments passed to \code{\link[stats]{glm.control}}.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{binary factor}, \code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[stats]{glm}}, \code{\link[stats]{glm.control}},
#' \code{\link[MASS]{stepAIC}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = GLMModel())
#' 
GLMModel <- function(family = NULL, ...) {
  
  args <- params(environment())
  is_main <- names(args) %in% "family"
  params <- args[is_main]
  params$control <- as.call(c(.(list), args[!is_main]))
  
  MLModel(
    name = "GLMModel",
    label = "Generalized Linear Models",
    packages = "stats",
    types = c("binary", "numeric"),
    params = params,
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, family = NULL, ...) {
      environment(formula) <- environment()
      if (is.null(family)) {
        family <- switch_class(response(formula, data),
                               "factor" = "binomial",
                               "numeric" = "gaussian")
      }
      stats::glm(formula, data = data, weights = weights, family = family, ...)
    },
    predict = function(object, newdata, ...) {
      predict(object, newdata = newdata, type = "response")
    },
    varimp = function(object, ...) varimp_pchisq(object)
  )
  
}


#' @name GLMStepAICModel
#' @rdname GLMModel
#' 
#' @param direction mode of stepwise search, can be one of \code{"both"}
#' (default), \code{"backward"}, or \code{"forward"}.
#' @param scope defines the range of models examined in the stepwise search.
#' This should be a list containing components \code{upper} and
#' \code{lower}, both formulae.
#' @param k multiple of the number of degrees of freedom used for the penalty.
#' Only \code{k = 2} gives the genuine AIC: \code{k = log(nobs)} is sometimes
#' referred to as BIC or SBC.
#' @param trace if positive, information is printed during the running of
#' \code{stepAIC}. Larger values may give more information on the fitting
#' process.
#' @param steps maximum number of steps to be considered.
#'
GLMStepAICModel <- function(family = NULL, ...,
                            direction = c("both", "backward", "forward"),
                            scope = NULL, k = 2, trace = FALSE, steps = 1000) {
  
  direction <- match.arg(direction)
  
  args <- params(environment())
  is_step <- names(args) %in% c("direction", "scope", "k", "trace", "steps")
  params <- args[is_step]
  
  stepmodel <- GLMModel(family = family, ...)
  
  MLModel(
    name = "GLMStepAICModel",
    label = "Generalized Linear Models (Stepwise)",
    packages = c(stepmodel@packages, "MASS"),
    types = stepmodel@types,
    params = c(stepmodel@params, params),
    nvars = stepmodel@nvars,
    fit = function(formula, data, weights, family = NULL, direction = "both",
                   scope = list(), k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      if (is.null(family)) {
        family <- switch_class(response(formula, data),
                               "factor" = "binomial",
                               "numeric" = "gaussian")
      }
      stepargs <- stepAIC_args(formula, direction, scope)
      stats::glm(stepargs$formula, data = data, weights = weights,
                 family = family, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps)
    },
    predict = fitbit(stepmodel, "predict"),
    varimp = fitbit(stepmodel, "varimp")
  )
  
}
