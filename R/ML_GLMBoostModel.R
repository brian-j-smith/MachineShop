#' Gradient Boosting with Linear Models
#' 
#' Gradient boosting for optimizing arbitrary loss functions where
#' component-wise linear models are utilized as base-learners.
#' 
#' @param family \code{\link[mboost]{Family}} object.  Set automatically
#' according to the class type of the response variable.
#' @param mstop number of initial boosting iterations.
#' @param nu step size or shrinkage parameter between 0 and 1.
#' @param risk method to use in computing the empirical risk for each boosting
#' iteration.
#' @param stopintern logical inidicating whether the boosting algorithm stops
#' internally when the out-of-bag risk increases at a subsequent iteration.
#' @param trace logical indicating whether status information is printed during
#' the fitting process.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{binary}, \code{numeric}, \code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[mboost]{glmboost}}, \code{\link[mboost]{Family}},
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(type ~ ., data = Pima.tr, model = GLMBoostModel())
#'
GLMBoostModel <- function(family = NULL, mstop = 100, nu = 0.1,
                          risk = c("inbag", "oobag", "none"),
                          stopintern = FALSE, trace = FALSE) {
  
  args <- params(environment())
  is_main <- names(args) %in% "family"
  params <- args[is_main]
  params$control <- as.call(c(.(mboost::boost_control), args[!is_main]))

  MLModel(
    name = "GLMBoostModel",
    packages = "mboost",
    types = c("binary", "numeric", "Surv"),
    params = params,
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, family = NULL, ...) {
      environment(formula) <- environment()
      if (is.null(family)) {
        family <- switch_class(response(formula, data),
                               "factor" = mboost::Binomial(),
                               "numeric" = mboost::Gaussian(),
                               "Surv" = mboost::CoxPH())
      }
      mboost::glmboost(formula, data = data, na.action = na.pass,
                       weights = weights, family = family, ...)
    },
    predict = function(object, newdata, times, ...) {
      if (object$family@name == "Cox Partial Likelihood") {
        new_neg_risk <-
          -exp(predict(object, newdata = newdata, type = "link")) %>% drop
        if (length(times)) {
          y <- object$response
          risk <- exp(predict(object, type = "link")) %>% drop
          cumhaz <- basehaz(y, risk, times)
          exp(new_neg_risk %o% cumhaz)
        } else {
          new_neg_risk
        }
      } else {
        predict(object, newdata = newdata, type = "response")
      }
    },
    varimp = function(object, ...) {
      structure(mboost::varimp(object), class = "numeric")
    }
  )
  
}
