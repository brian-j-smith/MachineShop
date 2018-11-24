#' Parametric Survival Model
#' 
#' Fits the accelerated failure time family of parametric survival models.
#' 
#' @param dist assumed distribution for y variable.
#' @param scale optional fixed value for the scale.
#' @param parms a list of fixed parameters.
#' @param control a list of control values, in the format produced by
#' \code{\link[survival]{survreg.control}}.
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
#' @seealso \code{\link[rms]{psm}}, \code{\link[survival]{survreg}}
#' 
SurvRegModel <- function(dist = c("weibull", "exponential", "gaussian",
                                  "logistic", "lognormal", "logloglogistic"),
                         scale = 0, parms = NULL, control = NULL) {
  dist <- match.arg(dist)
  MLModel(
    name = "SurvRegModel",
    packages = "rms",
    types = "Surv",
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      rms::psm(formula, data = data, weights = weights, ...)
    },
    predict = function(object, newdata, times, ...) {
      object <- unMLModelFit(object)
      if (length(times)) {
        pred <- rms::survest(object, newdata = newdata, times = times,
                             conf.int = FALSE)
        if (is(pred, "survest.psm")) as.matrix(pred$surv) else pred
      } else {
        exp(predict(object, newdata = newdata, type = "lp"))
      }
    },
    varimp = function(object, ...) {
      pchisq(coef(object)^2 / diag(vcov(object)), 1)
    }
  )
}


#' @name SurvRegStepAICModel
#' @rdname SurvRegModel
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
#' @examples
#' library(survival)
#' library(MASS)
#' 
#' fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'     data = Melanoma, model = SurvRegModel())
#'
SurvRegStepAICModel <- function(dist = c("weibull", "exponential", "gaussian",
                                         "logistic", "lognormal",
                                         "logloglogistic"),
                                scale = 0, parms = NULL, control = NULL,
                                direction = c("both", "backward", "forward"),
                                scope = NULL, k = 2, trace = FALSE,
                                steps = 1000) {
  dist <- match.arg(dist)
  direction <- match.arg(direction)
  args <- params(environment())
  stepmodel <- SurvRegModel(dist = dist, scale = scale, parms = parms,
                            control = control)
  MLModel(
    name = "SurvRegStepAICModel",
    packages = c("MASS", stepmodel@packages),
    types = stepmodel@types,
    params = args,
    nvars = stepmodel@nvars,
    fit = function(formula, data, weights, direction = "both", scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      stepargs <- stepAIC_args(formula, direction, scope)
      rms::psm(stepargs$formula, data = data, weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps)
    },
    predict = fitbit(stepmodel, "predict"),
    varimp = fitbit(stepmodel, "varimp")
  )
}
