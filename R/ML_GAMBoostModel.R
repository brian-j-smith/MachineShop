#' Gradient Boosting with Additive Models
#' 
#' Gradient boosting for optimizing arbitrary loss functions, where
#' component-wise arbitrary base-learners, e.g., smoothing procedures, are
#' utilized as additive base-learners.
#' 
#' @param family \code{\link[mboost]{Family}} object.  Set automatically
#' according to the class type of the response variable.
#' @param baselearner character specifying the component-wise
#' \code{\link[mboost:baselearners]{base learner}} to be used.
#' @param dfbase gobal degrees of freedom for P-spline base learners
#' (\code{"bbs"}).
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
#' @seealso \code{\link[mboost]{gamboost}}, \code{\link[mboost]{Family}},
#' \code{\link[mboost]{baselearners}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(type ~ ., data = Pima.tr, model = GAMBoostModel())
#'
GAMBoostModel <- function(family = NULL,
                          baselearner = c("bbs", "bols", "btree", "bss", "bns"),
                          dfbase = 4, mstop = 100, nu = 0.1,
                          risk = c("inbag", "oobag", "none"),
                          stopintern = FALSE, trace = FALSE) {
  
  baselearner <- match.arg(baselearner)
  risk <- match.arg(risk)
  
  args <- params(environment())
  mainargs <- names(args) %in% c("family", "baselearner", "dfbase")
  params <- args[mainargs]
  params$control <- as.call(c(.(mboost::boost_control), args[!mainargs]))

  MLModel(
    name = "GAMBoostModel",
    packages = "mboost",
    types = c("binary", "numeric", "Surv"),
    params = params,
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, family = NULL, ...) {
      attachment(list(
        bbs = mboost::bbs,
        bols = mboost::bols,
        btree = mboost::btree,
        bbs = mboost::bbs,
        bns = mboost::bns
      ), name = "mboost_exports")
      environment(formula) <- environment()
      if (is.null(family)) {
        family <- switch_class(response(formula, data),
                               "factor" = mboost::Binomial(),
                               "numeric" = mboost::Gaussian(),
                               "Surv" = mboost::CoxPH())
      }
      mboost::gamboost(formula, data = data, na.action = na.pass,
                       weights = weights, family = family, ...)
    },
    predict = function(object, newdata, times, ...) {
      if (object$family@name == "Cox Partial Likelihood") {
        newlp <- predict(object, newdata = newdata, type = "link") %>% drop
        if (length(times)) {
          y <- object$response
          lp <- predict(object, type = "link") %>% drop
          cumhaz <- basehaz(y, exp(lp), times)
          exp(exp(newlp) %o% -cumhaz)
        } else {
          exp(newlp)
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
