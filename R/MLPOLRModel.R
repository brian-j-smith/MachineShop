POLRModel <- function(method = NULL) {
  MLModel(
    name = "POLRModel",
    packages = "MASS",
    responses = "ordered",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      MASS::polr(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("POLRFit", POLRModel(...))
    },
    predict = function(object, newdata, ...) {
      object <- asParentFit(object)
      predict(object, newdata = newdata, type = "probs")
    }
  )
}
