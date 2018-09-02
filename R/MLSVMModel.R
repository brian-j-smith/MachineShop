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
      kernlab::predict(asParentFit(object), newdata = newdata,
                       type = ifelse(is.factor(response(object)),
                                     "probabilities", "response"))
    },
    response = function(object, ...) {
      if(is.character(object@lev)) {
        factor(object@ymatrix, levels = 1:object@nclass, labels = object@lev)
      } else {
        object@ymatrix
      }
    },
    varimp = function(object, ...) {
      stop("variable importance not available")
    }
  )
}
