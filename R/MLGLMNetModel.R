#' GLM Lasso or Elasticnet Model
#'
#' Fit a generalized linear model via penalized maximum likelihood.
#'
#' @param family response type.  Set automatically according to the class type
#' of the response variable.
#' @param alpha elasticnet mixing parameter.
#' @param lambda regularization parameter.
#' @param standardize logical flag for predictor variable standardization, prior
#' to model fitting.
#' @param intercept logical indicating whether to fit intercepts.
#' @param penalty.factor vector of penalty factors to be applied to each
#' coefficient.
#' @param standardize.response logical indicating whether to standardize
#' \code{"mgaussian"} response variables.
#' @param thresh convergence threshold for coordinate descent.
#' @param maxit maximum number of passes over the data for all lambda values.
#' @param type.gaussian algorithm type for guassian models.
#' @param type.logistic algorithm type for logistic models.
#' @param type.multinomial algorithm type for multinomial models.
#' 
#' @details 
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = GLMNetModel(lambda = 0.01))
#'
GLMNetModel <- function(family = NULL, alpha = 1, lambda = 0,
                        standardize = TRUE, intercept = NULL,
                        penalty.factor = .(rep(1, nvars)),
                        standardize.response = FALSE,
                        thresh = 1e-7, maxit = 100000,
                        type.gaussian =
                          .(ifelse(nvars < 500, "covariance", "naive")),
                        type.logistic = c("Newton", "modified.Newton"),
                        type.multinomial = c("ungrouped", "grouped")) {
  type.logistic <- match.arg(type.logistic)
  type.multinomial <- match.arg(type.multinomial)
  MLModel(
    name = "GLMNetModel",
    packages = "glmnet",
    types = c("factor", "numeric", "Surv"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, family = NULL, ...) {
      mf <- model.frame(formula, data, na.action = NULL)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      if (is.null(family)) {
        family <- switch_class(y,
                               "factor" = ifelse(nlevels(y) == 2,
                                                 "binomial", "multinomial"),
                               "numeric" = "gaussian",
                               "Surv" = "cox")
      }
      mfit <- glmnet::glmnet(x, y, weights = weights, family = family,
                             nlambda = 1, ...)
      mfit$x <- x
      mfit$y <- y
      mfit$formula <- formula
      mfit
    },
    predict = function(object, newdata, times = numeric(), ...) {
      x <- object$x
      y <- object$y
      fo <- object$formula[-2]
      object <- unMLModelFit(object)
      newmf <- model.frame(fo, newdata, na.action = NULL)
      newx <- model.matrix(fo, newmf)[, -1, drop = FALSE]
      if (is.Surv(y)) {
        if (length(times)) {
          lp <- predict(object, newx = x, type = "link") %>% drop
          newlp <- predict(object, newx = newx, type = "link") %>% drop
          cumhaz <- basehaz(y, exp(lp), times)
          exp(exp(newlp - mean(lp)) %o% -cumhaz)
        } else {
          exp(predict(object, newx = newx, type = "link")) %>% drop
        }
      } else {
        predict(object, newx = newx, type = "response") %>% drop
      }
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      convert <- function(x) abs(drop(as.matrix(x)))
      beta <- object$beta
      if (is.list(beta)) {
        as.data.frame(lapply(beta, convert), check.names = FALSE)
      } else {
        convert(beta)
      }
    }
  )
}
