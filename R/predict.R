#' Model Prediction
#' 
#' Predict outcomes with a fitted model.
#' 
#' @name predict
#' 
#' @param object MLModelFit object from a model fit.
#' @param newdata data frame with which to obtain predictions.
#' @param type specifies prediction on the original outcome scale
#' (\code{"response"}) or on a probability distribution scale (\code{"prob"}).
#' @param cutoff threshold above which probabilities are classified as success
#' for factor outcomes and which expected values are rounded for integer
#' outcomes.
#' @param times numeric vector of follow-up times at which to predict
#' survival events.
#' @param ... arguments passed to model-specific prediction functions.
#' 
#' @seealso \code{\link{fit}}, \code{\link{modelmetrics}}
#' 
#' @examples
#' ## Survival analysis example
#' library(survival)
#' 
#' gbmfit <- fit(Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
#'                                    pat.karno + meal.cal + wt.loss,
#'               data = lung,
#'               GBMModel)
#' predict(gbmfit, lung, times = c(180, 360, 540), type = "prob")
#' 
predict.MLModelFit <- function(object, newdata, type = c("response", "prob"),
                               cutoff = 0.5, times = NULL, ...) {
  if (missing(newdata)) stop("newdata is missing")
  pred <- .predict(object, newdata, times = times)
  if (match.arg(type) == "response") {
    pred <- convert(response(object), pred, cutoff = cutoff)
  }
  pred
}


.predict <- function(object, ...) {
  UseMethod(".predict", object)
}


.predict.MLModelFit <- function(object, newdata, times, ...) {
  requireModelNamespaces(field(object, ".packages"))
  predict <- field(object, ".predict")
  predict(object, newdata, times = times)
}


predict.survfit <- function(object, times, ...) {
  survtimes <- c(0, object$time)
  survprobs <- c(1, object$surv)
  idx <- sapply(times, function(x) max(which(survtimes <= x)))
  survprobs[idx]
}
