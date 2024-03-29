#' Proportional Hazards Regression Model
#'
#' Fits a Cox proportional hazards regression model. Time dependent variables,
#' time dependent strata, multiple events per subject, and other extensions are
#' incorporated using the counting process formulation of Andersen and Gill.
#'
#' @rdname CoxModel
#'
#' @param ties character string specifying the method for tie handling.
#' @param ... arguments passed to \code{\link[survival]{coxph.control}}.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{Surv}}
#' }
#'
#' Default argument values and further model details can be found in the source
#' See Also links below.
#'
#' In calls to \code{\link{varimp}} for \code{CoxModel} and
#' \code{CoxStepAICModel}, numeric argument \code{base} may be specified for the
#' (negative) logarithmic transformation of p-values [defaul: \code{exp(1)}].
#' Transformed p-values are automatically scaled in the calculation of variable
#' importance to range from 0 to 100.  To obtain unscaled importance values, set
#' \code{scale = FALSE}.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[survival]{coxph}},
#' \code{\link[survival]{coxph.control}}, \code{\link[MASS]{stepAIC}},
#' \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' library(survival)
#'
#' fit(Surv(time, status) ~ ., data = veteran, model = CoxModel)
#'
CoxModel <- function(ties = c("efron", "breslow", "exact"), ...) {

  ties <- match.arg(ties)

  MLModel(

    name = "CoxModel",
    label = "Cox Regression",
    packages = "survival",
    response_types = "Surv",
    weights = TRUE,
    predictor_encoding = "model.matrix",
    na.rm = TRUE,
    params = new_params(environment(), ...),

    fit = function(formula, data, weights, ...) {
      survival::coxph(
        formula, data = as.data.frame(formula, data = data), weights = weights,
        na.action = na.pass, ...
      )
    },

    predict = function(object, newdata, .MachineShop, ...) {
      y <- object$y
      weights <- case_weights(.MachineShop$input)
      newdata <- as.data.frame(newdata)
      lp <- predict(object, type = "lp")
      new_lp <- predict(object, newdata = newdata, type = "lp")
      predict(y, lp, new_lp, weights = weights, ...)
    },

    varimp = function(object, base = exp(1), ...) {
      varimp_pval(object, base = base)
    }

  )

}

MLModelFunction(CoxModel) <- NULL


#' @rdname CoxModel
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
CoxStepAICModel <- function(
  ties = c("efron", "breslow", "exact"), ...,
  direction = c("both", "backward", "forward"), scope = list(), k = 2,
  trace = FALSE, steps = 1000
) {

  direction <- match.arg(direction)

  params <- new_params(environment())
  stepmodel <- CoxModel(ties = ties, ...)
  params <- params[setdiff(names(params), names(stepmodel@params))]

  MLModel(

    name = "CoxStepAICModel",
    label = "Cox Regression (Stepwise)",
    packages = c(stepmodel@packages, "MASS"),
    response_types = stepmodel@response_types,
    weights = stepmodel@weights,
    predictor_encoding = stepmodel@predictor_encoding,
    na.rm = stepmodel@na.rm,
    params = c(stepmodel@params, params),

    fit = function(
      formula, data, weights, direction, scope = list(), k, trace, steps, ...
    ) {
      data <- as.data.frame(formula, data = data)
      stepargs <- stepAIC_args(formula, direction, scope)
      MASS::stepAIC(
        survival::coxph(
          stepargs$formula, data = data, weights = weights, na.action = na.pass,
          ...
        ),
        direction = direction, scope = stepargs$scope, k = k, trace = trace,
        steps = steps
      )
    },

    predict = stepmodel@predict,

    varimp = stepmodel@varimp

  )

}

MLModelFunction(CoxStepAICModel) <- NULL
