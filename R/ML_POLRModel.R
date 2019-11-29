#' Ordered Logistic or Probit Regression Model
#'
#' Fit a logistic or probit regression model to an ordered factor response.
#'
#' @param method logistic or probit or (complementary) log-log or cauchit
#' (corresponding to a Cauchy latent variable).
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{ordered}}
#' }
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[MASS]{polr}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' library(MASS)
#'
#' df <- within(Boston,
#'              medv <- cut(medv,
#'                          breaks = c(0, 10, 15, 20, 25, 50),
#'                          ordered = TRUE))
#' fit(medv ~ ., data = df, model = POLRModel)
#'
POLRModel <- function(method = c("logistic", "probit", "loglog", "cloglog",
                                 "cauchit")) {

  method <- match.arg(method)

  MLModel(
    name = "POLRModel",
    label = "Ordered Logistic Regression",
    packages = "MASS",
    response_types = "ordered",
    predictor_encoding = "model.matrix",
    params = params(environment()),
    fit = function(formula, data, weights, ...) {
      MASS::polr(formula, data = as.data.frame(data), weights = weights,
                 Hess = TRUE, ...)
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "probs")
    },
    varimp = function(object, ...) {
      beta_est <- coef(object)
      beta_var <- diag(vcov(object))[seq_along(beta_est)]
      varimp_pval(beta_est, beta_var)
    }
  )

}

MLModelFunction(POLRModel) <- NULL
