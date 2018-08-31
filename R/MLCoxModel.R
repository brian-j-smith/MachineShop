CoxModel <- function(ties = NULL, control = NULL) {
  MLModel(
    name = "CoxModel",
    packages = c("rms", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      rms::cph(formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      pred <- if(length(times)) {
        rms::survest(object, newdata = newdata, times = times,
                     conf.int = FALSE, se.fit = FALSE)$surv %>% as.matrix
      } else {
        exp(predict(object, newdata = newdata, type = "lp"))
      }
      if(type == "response") {
        pred <- convert(response(object), pred, cutoff = cutoff)
      }
      pred
    }
  )
}


CoxStepAICModel <- function(ties = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = NULL, steps = NULL)
  {
  MLModel(
    name = "CoxStepAICModel",
    packages = c("MASS", "rms", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 0, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      args <- argsStepAIC(formula, direction, scope)
      rms::cph(args$formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        MASS::stepAIC(direction = direction, scope = args$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    },
    predict = CoxModel()@predict
  )
}
