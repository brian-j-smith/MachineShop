#' GLM Lasso or Elasticnet Model
#'
#' Fit a generalized linear model via penalized maximum likelihood.
#'
#' @param family optional response type.  Set automatically according to the
#'   class type of the response variable.
#' @param alpha elasticnet mixing parameter.
#' @param lambda regularization parameter.  The default value \code{lambda = 0}
#'   performs no regularization and should be increased to avoid model fitting
#'   issues if the number of predictor variables is greater than the number of
#'   observations.
#' @param standardize logical flag for predictor variable standardization, prior
#'   to model fitting.
#' @param intercept logical indicating whether to fit intercepts.
#' @param penalty.factor vector of penalty factors to be applied to each
#'   coefficient.
#' @param standardize.response logical indicating whether to standardize
#'   \code{"mgaussian"} response variables.
#' @param thresh convergence threshold for coordinate descent.
#' @param maxit maximum number of passes over the data for all lambda values.
#' @param type.gaussian algorithm type for guassian models.
#' @param type.logistic algorithm type for logistic models.
#' @param type.multinomial algorithm type for multinomial models.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{BinomialVariate}, \code{factor},
#'     \code{matrix}, \code{numeric}, \code{PoissonVariate}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{lambda}, \code{alpha}
#'   }
#' }
#'
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = GLMNetModel(lambda = 0.01))
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
    label = "Lasso and Elastic-Net",
    packages = "glmnet",
    response_types = c("BinomialVariate", "factor", "matrix", "numeric",
                       "PoissonVariate", "Surv"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    grid = function(x, length, ...) {
      model <- GLMNetModel(lambda = NULL)
      model@params$nlambda <- 3
      modelfit <- fit(x, model = model)
      list(
        lambda = exp(seq(log(min(modelfit$lambda)),
                         log(max(modelfit$lambda)),
                         length = length)),
        alpha = seq(0.1, 1, length = length)
      )
    },
    fit = function(formula, data, weights, family = NULL, nlambda = 1, ...) {
      x <- model.matrix(data, intercept = FALSE)
      offset <- model.offset(data)
      y <- response(data)
      if (is.null(family)) {
        family <- switch_class(y,
                               BinomialVariate = "binomial",
                               factor = ifelse(nlevels(y) == 2,
                                               "binomial", "multinomial"),
                               matrix = "mgaussian",
                               numeric = "gaussian",
                               PoissonVariate = "poisson",
                               Surv = "cox")
      }
      glmnet::glmnet(x, y, offset = offset, weights = weights,
                     family = family, nlambda = nlambda, ...)
    },
    predict = function(object, newdata, model, times, ...) {
      newx <- model.matrix(newdata, intercept = FALSE)
      newoffset <- model.offset(newdata)
      y <- response(model)
      if (is.Surv(y)) {
        data <- predictor_frame(model)
        lp <- predict(object,
                      newx = model.matrix(data, intercept = FALSE),
                      newoffset = model.offset(data),
                      type = "link")[, 1]
        new_lp <- predict(object, newx = newx, newoffset = newoffset,
                          type = "link")[, 1]
        predict(y, lp, times, new_lp, ...)
      } else {
        predict(object, newx = newx, newoffset = newoffset,
                s = object$lambda[1], type = "response")
      }
    },
    varimp = function(object, ...) {
      convert <- function(x) abs(drop(as.matrix(x)))
      beta <- object$beta
      if (is.list(beta)) {
        as.data.frame(map(convert, beta), check.names = FALSE)
      } else {
        convert(beta)
      }
    }
  )

}

MLModelFunction(GLMNetModel) <- NULL
