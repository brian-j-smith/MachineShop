GLMModel <- function(family = NULL, control = NULL) {
  MLModel(
    name = "GLMModel",
    packages = "stats",
    responses = c("factor", "numeric"),
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


GLMStepAICModel <- function(family = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = FALSE, steps = NULL)
  {
  MLModel(
    name = "GLMStepAICModel",
    packages = c("MASS", "stats"),
    responses = c("factor", "numeric"),
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
      stepargs <- argsStepAIC(formula, direction, scope)
      stats::glm(stepargs$formula, data = data, family = family,
                 weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("GLMFit", GLMModel(...))
    }
  )
}
