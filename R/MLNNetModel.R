NNetModel <- function(size, linout = NULL, entropy = NULL, softmax = NULL,
                      censored = NULL, skip = NULL, rang = NULL, decay = NULL,
                      maxit = NULL, Hess = NULL, trace = NULL, MaxNWts = NULL,
                      abstol = NULL, reltol = NULL) {
  MLModel(
    name = "NNetModel",
    packages = "nnet",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      mfit <- nnet::nnet(formula, data = data, weights = weights, trace = FALSE,
                         ...)
      mfit$y <- response(formula, data)
      asMLModelFit(mfit, "NNetFit", NNetModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(asParentFit(object), newdata = newdata, type = "raw")
    },
    response = function(object, ...) {
      object$y
    }
  )
}
