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
#'   \item{Response types:}{\code{Surv}}
#' }
#'
#' Default argument values and further model details can be found in the source
#' See Also links below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[rms]{psm}}, \code{\link[survival]{survreg}},
#' \code{\link[survival]{survreg.control}}, \code{\link[MASS]{stepAIC}},
#' \code{\link{fit}}, \code{\link{resample}}
#'
SurvRegModel <- function(
  dist = c("weibull", "exponential", "gaussian", "logistic", "lognormal",
           "logloglogistic"),
  scale = 0, parms = list(), ...
) {

  dist <- match.arg(dist)

  MLModel(

    name = "SurvRegModel",
    label = "Parametric Survival",
    packages = c("rms", "Hmisc"),
    response_types = "Surv",
    weights = TRUE,
    predictor_encoding = "model.matrix",
    na.rm = TRUE,
    params = new_params(environment(), ...),

    fit = function(formula, data, weights, dist, scale, parms = NULL, ...) {
      rms::psm(
        formula, data = as.data.frame(formula, data = data), weights = weights,
        na.action = na.pass, dist = dist, scale = scale, parms = parms,
        control = survival::survreg.control(...)
      )
    },

    predict = function(object, newdata, times, ...) {
      newdata <- as.data.frame(newdata)
      pred <- if (length(times)) {
        rms::survest(object, newdata = newdata, times = times, conf.int = FALSE)
      } else {
        Hmisc::Mean(object)(predict(object, newdata = newdata, type = "lp"))
      }
      if (is(pred, "survest.psm")) pred <- as.matrix(pred$surv)
      SurvPrediction(pred, times = times, distr = object$dist)
    },

    varimp = function(object, base = exp(1), ...) {
      varimp_pval(object, base = base)
    }

  )

}

MLModelFunction(SurvRegModel) <- NULL


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
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested packages rms and Hmisc to run
#'
#' library(survival)
#'
#' fit(Surv(time, status) ~ ., data = veteran, model = SurvRegModel)
#' }
#'
SurvRegStepAICModel <- function(
  dist = c("weibull", "exponential", "gaussian", "logistic", "lognormal",
           "logloglogistic"),
  scale = 0, parms = list(), ...,
  direction = c("both", "backward", "forward"), scope = list(), k = 2,
  trace = FALSE, steps = 1000
) {

  direction <- match.arg(direction)

  params <- new_params(environment())
  stepmodel <- SurvRegModel(dist = dist, scale = scale, parms = parms, ...)
  params <- params[setdiff(names(params), names(stepmodel@params))]

  MLModel(

    name = "SurvRegStepAICModel",
    label = "Parametric Survival (Stepwise)",
    packages = c(stepmodel@packages, "MASS"),
    response_types = stepmodel@response_types,
    weights = stepmodel@weights,
    predictor_encoding = stepmodel@predictor_encoding,
    na.rm = stepmodel@na.rm,
    params = c(stepmodel@params, params),

    fit = function(
      formula, data, weights, dist, scale, parms = NULL, ...,
      direction, scope = list(), k, trace, steps
    ) {
      data <- as.data.frame(formula, data = data)
      stepargs <- stepAIC_args(formula, direction, scope)
      MASS::stepAIC(
        rms::psm(
          stepargs$formula, data = data, weights = weights,
          na.action = na.pass, dist = dist, scale = scale, parms = parms,
          control = survival::survreg.control(...)
        ),
        direction = direction, scope = stepargs$scope, k = k, trace = trace,
        steps = steps
      )
    },

    predict = stepmodel@predict,

    varimp = stepmodel@varimp

  )

}

MLModelFunction(SurvRegStepAICModel) <- NULL
