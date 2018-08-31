SVMModel <- function(scaled = NULL, type = NULL, kernel = NULL, kpar = NULL,
                      C = NULL, nu = NULL, epsilon = NULL, cache = NULL,
                      tol = NULL, shrinking = NULL) {
  MLModel(
    name = "SVMModel",
    packages = "kernlab",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      environment(formula) <- environment()
      kernlab::ksvm(formula, data = data, prob.model = TRUE, ...) %>%
        asMLModelFit("SVMFit", SVMModel(...))
    },
    predict = function(object, newdata, ...) {
      obs <- response(object)
      object <- asParentFit(object)
      pred <- kernlab::predict(object, newdata = newdata,
                               type = ifelse(is.factor(obs),
                                             "probabilities", "response"))
    }
  )
}
