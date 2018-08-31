C50Model <- function(trials = NULL, rules = NULL, control = NULL, costs = NULL)
  {
  MLModel(
    name = "C50Model",
    packages = "C50",
    responses = "factor",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      mfit <- C50::C5.0(formula, data = data, weights = weights, ...)
      mfit$y <- response(formula, data)
      asMLModelFit(mfit, "C50Fit", C50Model(...))
    },
    predict = function(object, newdata, ...) {
      object <- asParentFit(object)
      predict(object, newdata = newdata, type = "prob")
    }
  )
}
