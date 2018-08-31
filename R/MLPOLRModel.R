POLRModel <- function(method = NULL) {
  MLModel(
    name = "POLRModel",
    packages = "MASS",
    responses = "ordered",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      MASS::polr(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("POLRModel", POLRModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, type = "probs")
      if(type == "response") {
        pred <- convert(factor(object$lev), pred, cutoff = cutoff)
      }
      pred
    }
  )
}
