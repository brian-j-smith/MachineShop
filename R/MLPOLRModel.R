#' Ordered Logistic or Probit Regression Model
#' 
#' Fit a logistic or probit regression model to an ordered factor response.
#' 
#' @param method logistic or probit or (complementary) log-log or cauchit
#' (corresponding to a Cauchy latent variable).
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{ordered}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[MASS]{polr}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
POLRModel <- function(method = NULL) {
  MLModel(
    name = "POLRModel",
    packages = "MASS",
    types = "ordered",
    params = params(environment()),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      MASS::polr(formula, data = data, weights = weights, Hess = TRUE, ...) %>%
        asMLModelFit("POLRFit", POLRModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(unMLModelFit(object), newdata = newdata, type = "probs")
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
