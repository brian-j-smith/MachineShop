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
predict.MLModelFit <- function(object, newdata, type = c("response", "prob"),
                               cutoff = 0.5, times = NULL, ...) {
  type <- match.arg(type)
  obs <- response(object)
  pred <- predict_sub(object, newdata, times = times)
  if(type == "response") convert(obs, pred, cutoff = cutoff) else pred
}


predict_sub <- function(object, ...) {
  UseMethod("predict_sub", object)
}


predict_sub.MLModelFit <- function(object, newdata, times, ...) {
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
