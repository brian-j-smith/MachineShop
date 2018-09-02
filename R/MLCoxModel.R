CoxModel <- function(ties = NULL, control = NULL) {
  MLModel(
    name = "CoxModel",
    packages = "rms",
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      rms::cph(formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      object <- asParentFit(object)
      if(length(times)) {
        rms::survest(object, newdata = newdata, times = times,
                     conf.int = FALSE, se.fit = FALSE)$surv %>% as.matrix
      } else {
        exp(predict(object, newdata = newdata, type = "lp"))
      }
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      pchisq(coef(object)^2 / diag(vcov(object)), 1)
    }
  )
}


CoxStepAICModel <- function(ties = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = FALSE, steps = NULL)
  {
  MLModel(
    name = "CoxStepAICModel",
    packages = c("MASS", "rms"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      args <- argsStepAIC(formula, direction, scope)
      rms::cph(args$formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        MASS::stepAIC(direction = direction, scope = args$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    }
  )
}
