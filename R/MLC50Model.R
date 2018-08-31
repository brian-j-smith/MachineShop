C50Model <- function(trials = NULL, rules = NULL, control = NULL, costs = NULL)
  {
  MLModel(
    name = "C50Model",
    packages = "C50",
    responses = "factor",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      C50::C5.0(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("C50Fit", C50Model(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, type = "prob")
      if(type == "response") {
        pred <- convert(factor(object$levels), pred, cutoff = cutoff)
      }
      pred
    }
  )
}
