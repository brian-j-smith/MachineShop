POLRModel <- function(method = NULL) {
  MLModel(
    name = "POLRModel",
    packages = "MASS",
    responses = "ordered",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      MASS::polr(formula, data = data, weights = weights, Hess = TRUE, ...) %>%
        asMLModelFit("POLRFit", POLRModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(asParentFit(object), newdata = newdata, type = "probs")
    },
    response = function(object, ...) {
      object$model[[1]]
    },
    varimp = function(object, ...) {
      beta <- coef(object)
      s2 <- head(diag(vcov(object)), length(beta))
      pchisq(beta^2 / s2, 1)
    }
  )
}
