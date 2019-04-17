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
#' df <- within(ICHomes,
#'              sale_amount <- cut(sale_amount,
#'              breaks = c(0, 100, 200, 300, 1000) * 1000,
#'              ordered = TRUE))
#' fit(sale_amount ~ ., data = df, model = POLRModel())
#' 
POLRModel <- function(method = c("logistic", "probit", "loglog", "cloglog",
                                 "cauchit")) {
  
  method <- match.arg(method)
  
  MLModel(
    name = "POLRModel",
    label = "Ordered Logistic Regression",
    packages = "MASS",
    types = "ordered",
    params = params(environment()),
    design = "model.matrix",
    fit = function(formula, data, weights, ...) {
      MASS::polr(formula, data = as.data.frame(data), weights = weights,
                 Hess = TRUE, ...)
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "probs")
    },
    varimp = function(object, ...) {
      beta <- coef(object)
      s2 <- diag(vcov(object))[seq_along(beta)]
      pchisq(beta^2 / s2, 1)
    }
  )
  
}
