#' Parametric Survival Model
#' 
#' Fits the accelerated failure time family of parametric survival models.
#' 
#' @rdname SurvRegModel
#' 
#' @param dist assumed distribution for y variable.
#' @param scale optional fixed value for the scale.
#' @param parms list of fixed parameters.
#' @param ... arguments passed to \code{\link[survival]{survreg.control}}.
#' 
#' @details
#' \describe{
#'   \item{Response Types:}{\code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#'
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[rms]{psm}}, \code{\link[survival]{survreg}},
#' \code{\link[survival]{survreg.control}}, \code{\link[MASS]{stepAIC}},
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
SurvRegModel <- function(dist = c("weibull", "exponential", "gaussian",
                                  "logistic", "lognormal", "logloglogistic"),
                         scale = NULL, parms = NULL, ...) {
  
  dist <- match.arg(dist)
  
  args <- params(environment())
  is_main <- names(args) %in% c("dist", "scale", "parms")
  params <- args[is_main]
  params$control <- as.call(c(.(survival::survreg.control), args[!is_main]))
  
  MLModel(
    name = "SurvRegModel",
    label = "Parametric Survival",
    packages = c("rms", "Hmisc"),
    response_types = "Surv",
    predictor_encoding = "model.matrix",
    params = params,
    fit = function(formula, data, weights, ...) {
      rms::psm(formula, data = as.data.frame(data), weights = weights, ...)
    },
    predict = function(object, newdata, times, ...) {
      newdata <- as.data.frame(newdata)
      if (length(times)) {
        pred <- rms::survest(object, newdata = newdata, times = times,
                             conf.int = FALSE)
        if (is(pred, "survest.psm")) as.matrix(pred$surv) else pred
      } else {
        Hmisc::Mean(object)(predict(object, newdata = newdata, type = "lp"))
      }
    },
    varimp = function(object, ...) varimp_wald(object)
  )
  
}


#' @rdname SurvRegModel
#' 
#' @param direction mode of stepwise search, can be one of \code{"both"}
#'   (default), \code{"backward"}, or \code{"forward"}.
#' @param scope defines the range of models examined in the stepwise search.
#'   This should be a list containing components \code{upper} and \code{lower},
#'   both formulae.
#' @param k multiple of the number of degrees of freedom used for the penalty.
#'   Only \code{k = 2} gives the genuine AIC; \code{k = .(log(nobs))} is
#'   sometimes referred to as BIC or SBC.
#' @param trace if positive, information is printed during the running of
#'   \code{stepAIC}. Larger values may give more information on the fitting
#'   process.
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
#'     data = Melanoma, model = SurvRegModel)
#'
SurvRegStepAICModel <- function(dist = c("weibull", "exponential", "gaussian",
                                         "logistic", "lognormal",
                                         "logloglogistic"),
                                scale = NULL, parms = NULL, ...,
                                direction = c("both", "backward", "forward"),
                                scope = NULL, k = 2, trace = FALSE,
                                steps = 1000) {
  
  direction <- match.arg(direction)
  
  args <- params(environment())
  is_step <- names(args) %in% c("direction", "scope", "k", "trace", "steps")
  params <- args[is_step]
  
  stepmodel <- SurvRegModel(dist = dist, scale = scale, parms = parms, ...)

  MLModel(
    name = "SurvRegStepAICModel",
    label = "Parametric Survival (Stepwise)",
    packages = c(stepmodel@packages, "MASS"),
    response_types = stepmodel@response_types,
    predictor_encoding = stepmodel@predictor_encoding,
    params = c(stepmodel@params, params),
    fit = function(formula, data, weights, direction = "both", scope = list(),
                   k = 2, trace = 1, steps = 1000, ...) {
      environment(formula) <- environment()
      stepargs <- stepAIC_args(formula, direction, scope)
      data <- as.data.frame(data)
      rms::psm(stepargs$formula, data = data, weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = stepargs$scope, k = k,
                      trace = trace, steps = steps)
    },
    predict = fitbit(stepmodel, "predict"),
    varimp = fitbit(stepmodel, "varimp")
  )
  
}
