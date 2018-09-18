#' Generalized Linear Model
#' 
#' Fits generalized linear models, specified by giving a symbolic description of
#' the linear predictor and a description of the error distribution.
#' 
#' @param family description of the error distribution and link function to be
#' used in the model.
#' @param control list of parameters for controlling the fitting process.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor} (two-levels), \code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[stats]{glm}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
GLMModel <- function(family = NULL, control = NULL) {
  MLModel(
    name = "GLMModel",
    packages = "stats",
    types = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      args <- list(...)
      family <- args$family
      if(is.null(family)) {
        family <- switch(class(response(formula, data)),
                         "factor" = "binomial",
                         "numeric" = "gaussian")
      }
      stats::glm(formula, data = data, family = family, weights = weights,
                 ...) %>%
        asMLModelFit("GLMFit", GLMModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(asParentFit(object), newdata = newdata, type = "response")
    },
    response = function(object, ...) {
      response(object$formula, object$data)
    },
    varimp = function(object, ...) {
      pchisq(coef(object)^2 / diag(vcov(object)), 1)
    }
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
#' Only \code{k = 2} gives the genuine AIC: \code{k = log(n)} is sometimes
#' referred to as BIC or SBC.
#' @param trace if positive, information is printed during the running of
#' \code{stepAIC}. Larger values may give more information on the fitting
#' process.
#' @param steps maximum number of steps to be considered.
#' 
#' @details Default values for the \code{NULL} arguments and further model
#' details can be found in the source links below.
#'
#' @seealso \code{\link[MASS]{stepAIC}}
#'
GLMStepAICModel <- function(family = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = FALSE, steps = NULL)
  {
  MLModel(
    name = "GLMStepAICModel",
    packages = c("MASS", "stats"),
    types = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      args <- list(...)
      family <- args$family
      if(is.null(family)) {
        family <- switch(class(response(formula, data)),
                         "factor" = "binomial",
                         "numeric" = "gaussian")
      }
      direction <- match.arg(direction)
      stepargs <- stepAIC_args(formula, direction, scope)
      stats::glm(stepargs$formula, data = data, family = family,
                 weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("GLMFit", GLMModel(...))
    }
  )
}
