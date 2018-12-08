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
#' Further model details can be found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[MASS]{polr}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' df <- Boston
#' df$medv <- cut(Boston$medv, breaks = c(0, 15, 20, 25, 50), ordered = TRUE)   
#' fit(medv ~ ., data = df, model = POLRModel())
#' 
POLRModel <- function(method = c("logistic", "probit", "loglog", "cloglog",
                                 "cauchit")) {
  
  method <- match.arg(method)
  
  MLModel(
    name = "POLRModel",
    packages = "MASS",
    types = "ordered",
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      MASS::polr(formula, data = data, weights = weights, Hess = TRUE, ...)
    },
    predict = function(object, newdata, ...) {
      predict(object, newdata = newdata, type = "probs")
    },
    varimp = function(object, ...) {
      beta <- coef(object)
      s2 <- diag(vcov(object))[seq_along(beta)]
      pchisq(beta^2 / s2, 1)
    }
  )
  
}
