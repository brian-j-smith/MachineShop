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
#' @details
#' \describe{
#' \item{Response Types:}{\code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#'
#' @return MLModel class object.
#' 
#' @seealso \code{\link[rms]{cph}}, \code{\link[survival]{coxph}}
#' 
#' @examples
#' library(survival)
#' 
#' fit(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + meal.cal + wt.loss,
#'     data = lung, model = CoxModel())
#' 
CoxModel <- function(ties = c("efron", "breslow", "exact"), control = NULL) {
  ties <- match.arg(ties)
  MLModel(
    name = "CoxModel",
    packages = "rms",
    types = "Surv",
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      rms::cph(formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...)
    },
    predict = function(object, newdata, times = numeric(), ...) {
      object <- unMLModelFit(object)
      if (length(times)) {
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
#' Only \code{k = 2} gives the genuine AIC: \code{k = log(nobs)} is sometimes
#' referred to as BIC or SBC.
#' @param trace if positive, information is printed during the running of
#' \code{stepAIC}. Larger values may give more information on the fitting
#' process.
#' @param steps maximum number of steps to be considered.
#' 
#' @seealso \code{\link[MASS]{stepAIC}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#'
CoxStepAICModel <- function(ties = c("efron", "breslow", "exact"),
                            control = NULL,
                            direction = c("both", "backward", "forward"),
                            scope = NULL, k = 2, trace = FALSE, steps = 1000)
  {
  ties <- match.arg(ties)
  direction <- match.arg(direction)
  args <- params(environment())
  stepmodel <- CoxModel(ties = ties, control = control)
  MLModel(
    name = "CoxStepAICModel",
    packages = c("MASS", "rms"),
    types = "Surv",
    params = args,
    nvars = stepmodel@nvars,
    fit = function(formula, data, weights, direction = "both", scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      stepargs <- stepAIC_args(formula, direction, scope)
      rms::cph(stepargs$formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps)
    },
    predict = stepmodel@predict,
    response = stepmodel@response,
    varimp = stepmodel@varimp
  )
}
