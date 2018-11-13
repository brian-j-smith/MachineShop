#' Model Prediction
#' 
#' Predict outcomes with a fitted model.
#' 
#' @name predict
#' 
#' @param object MLModelFit object from a model fit.
#' @param newdata optional data frame with which to obtain predictions.  If not
#' specified, the training data will be used by default.
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
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' gbmfit <- fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'               data = Melanoma, model = GBMModel)
#' predict(gbmfit, newdata = Melanoma, times = 365 * c(2, 5, 10), type = "prob")
#' 
predict.MLModelFit <- function(object, newdata = NULL,
                               type = c("response", "prob"), cutoff = 0.5,
                               times = numeric(), ...) {
  newdata <- preprocess(fitbit(object, "x"), newdata)
  requireModelNamespaces(fitbit(object, "packages"))
  predict <- fitbit(object, "predict")
  pred <- predict(object, newdata, times = times)
  if (match.arg(type) == "response") {
    pred <- convert(response(object), pred, cutoff = cutoff)
  }
  pred
}


predict.survfit <- function(object, times, ...) {
  survtimes <- c(0, object$time)
  survprobs <- c(1, object$surv)
  idx <- sapply(times, function(x) max(which(survtimes <= x)))
  survprobs[idx]
}
