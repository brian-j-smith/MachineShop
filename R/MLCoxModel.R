#' Proportional Hazards Regression Model
#'
#' Fits a Cox proportional hazards regression model. Time dependent variables,
#' time dependent strata, multiple events per subject, and other extensions are
#' incorporated using the counting process formulation of Andersen and Gill.
#' 
#' @param ties character string specifying the method for tie handling.
#' @param control object of class \code{\link[survival]{coxph.control}}
#' specifying iteration limit and other control options.
#' 
#' @seealso \code{\link[survival]{coxph}}
#' 
CoxModel <- function(ties = NULL, control = NULL) {
  MLModel(
    name = "CoxModel",
    packages = "rms",
    types = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      rms::cph(formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      object <- asParentFit(object)
      if(length(times)) {
        rms::survest(object, newdata = newdata, times = times,
                     conf.int = FALSE, se.fit = FALSE)$surv %>% as.matrix
      } else {
        exp(predict(object, newdata = newdata, type = "lp"))
      }
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      pchisq(coef(object)^2 / diag(vcov(object)), 1)
    }
  )
}


#' @name CoxStepAICModel
#' @rdname CoxModel
#' 
#' @param direction mode of stepwise search, can be one of \code{"both"}
#' (default), \code{"backward"}, or \code{"forward"}.
#' @param scope defines the range of models examined in the stepwise search.
#' This should be a list containing components \code{upper} and
#' \code{lower}, both formulae.
#' @param k multiple of the number of degrees of freedom used for the penalty.
#' Only \code{k = 2} gives the genuine AIC: \code{k = log(n)} is sometimes
#' referred to as BIC or SBC.
#' @param trace if positive, information is printed during the running of
#' \code{stepAIC}. Larger values may give more information on the fitting
#' process.
#' @param steps maximum number of steps to be considered.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source links below.
#'
#' @seealso \code{\link[MASS]{stepAIC}}
#'
CoxStepAICModel <- function(ties = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = FALSE, steps = NULL)
  {
  MLModel(
    name = "CoxStepAICModel",
    packages = c("MASS", "rms"),
    types = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      args <- argsStepAIC(formula, direction, scope)
      rms::cph(args$formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        MASS::stepAIC(direction = direction, scope = args$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    }
  )
}
