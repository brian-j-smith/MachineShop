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
#' @seealso \code{\link[rms]{psm}}, \code{\link[survival]{survreg}}
#' 
SurvRegModel <- function(dist = NULL, scale = NULL, parms = NULL,
                         control = NULL) {
  MLModel(
    name = "SurvRegModel",
    packages = "rms",
    types = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      rms::psm(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("SurvRegFit", SurvRegModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      object <- asParentFit(object)
      if(length(times)) {
        pred <- rms::survest(object, newdata = newdata, times = times,
                             conf.int = FALSE)
        if(inherits(pred, "survest.psm")) as.matrix(pred$surv) else pred
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


#' @name SurvRegStepAICModel
#' @rdname SurvRegModel
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
#' @return MLModel class object.
#' 
#' @seealso \code{\link[MASS]{stepAIC}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#'
SurvRegStepAICModel <- function(dist = NULL, scale = NULL, parms = NULL,
                                control = NULL, direction = NULL, scope = NULL,
                                k = NULL, trace = FALSE, steps = NULL) {
  MLModel(
    name = "SurvRegStepAICModel",
    packages = c("MASS", "rms"),
    types = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      stepargs <- stepAIC_args(formula, direction, scope)
      rms::psm(stepargs$formula, data = data, weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("SurvRegFit", SurvRegModel(...))
    }
  )
}
