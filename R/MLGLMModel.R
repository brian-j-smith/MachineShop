GLMModel <- function(family = NULL, control = NULL) {
  MLModel(
    name = "GLMModel",
    packages = "stats",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      stats::glm(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("GLMFit", GLMModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, type = "response")
      if(type == "response") {
        pred <- convert(response(object), pred, cutoff = cutoff)
      }
      pred
    }
  )
}


GLMStepAICModel <- function(family = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = NULL, steps = NULL)
  {
  MLModel(
    name = "GLMStepAICModel",
    packages = c("MASS", "stats"),
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 0, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      args <- argsStepAIC(formula, direction, scope)
      stats::glm(args$formula, data = data, weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = args$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("GLMFit", GLMModel(...))
    },
    predict = GLMModel()@predict
  )
}
