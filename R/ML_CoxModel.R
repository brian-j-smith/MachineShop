#' Proportional Hazards Regression Model
#'
#' Fits a Cox proportional hazards regression model. Time dependent variables,
#' time dependent strata, multiple events per subject, and other extensions are
#' incorporated using the counting process formulation of Andersen and Gill.
#' 
#' @param ties character string specifying the method for tie handling.
#' @param ... arguments passed to \code{\link[survival]{coxph.control}}.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#'
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[survival]{coxph}},
#' \code{\link[survival]{coxph.control}}, \code{\link[MASS]{stepAIC}},
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' library(survival)
#' library(MASS)
#' 
#' fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'     data = Melanoma, model = CoxModel)
#' 
CoxModel <- function(ties = c("efron", "breslow", "exact"), ...) {
  
  ties <- match.arg(ties)
  
  args <- params(environment())
  is_main <- names(args) %in% c("ties", "eps", "iter.max")
  params <- args[is_main]
  params$tol <- args$toler.chol

  MLModel(
    name = "CoxModel",
    label = "Cox Regression",
    packages = "survival",
    response_types = "Surv",
    predictor_encoding = "model.matrix",
    params = params,
    fit = function(formula, data, weights, ...) {
      survival::coxph(formula, data = as.data.frame(data), weights = weights,
                      ...)
    },
    predict = function(object, newdata, times, ...) {
      y <- object$y
      newdata <- as.data.frame(newdata)
      lp <- predict(object, type = "lp")
      new_lp <- predict(object, newdata = newdata, type = "lp")
      predict(y, lp, times, new_lp, ...)
    },
    varimp = function(object, ...) varimp_wald(object)
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
CoxStepAICModel <- function(ties = c("efron", "breslow", "exact"), ...,
                            direction = c("both", "backward", "forward"),
                            scope = NULL, k = 2, trace = FALSE, steps = 1000) {
  
  direction <- match.arg(direction)
  
  args <- params(environment())
  is_step <- names(args) %in% c("direction", "scope", "k", "trace", "steps")
  params <- args[is_step]

  stepmodel <- CoxModel(ties = ties, ...)
  
  MLModel(
    name = "CoxStepAICModel",
    label = "Cox Regression (Stepwise)",
    packages = c(stepmodel@packages, "MASS"),
    response_types = stepmodel@response_types,
    predictor_encoding = stepmodel@predictor_encoding,
    params = c(stepmodel@params, params),
    fit = function(formula, data, weights, direction = "both", scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      stepargs <- stepAIC_args(formula, direction, scope)
      data <- as.data.frame(data)
      survival::coxph(stepargs$formula, data = data, weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps)
    },
    predict = fitbit(stepmodel, "predict"),
    varimp = fitbit(stepmodel, "varimp")
  )
  
}
