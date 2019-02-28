#' Model Prediction
#' 
#' Predict outcomes with a fitted model.
#' 
#' @name predict
#' 
#' @param object \code{MLModelFit} object from a model fit.
#' @param newdata optional \code{data.frame} with which to obtain predictions.
#' If not specified, the training data will be used by default.
#' @param type specifies prediction on the original outcome scale
#' (\code{"response"}) or on a probability distribution scale (\code{"prob"}).
#' @param cutoff threshold above which binary factor probabilities are
#' classified as events, below which survival probabilities are classified, and
#' at which expected values are rounded for integer outcomes.
#' @param times numeric vector of follow-up times at which to predict
#' survival events/probabilities.
#' @param method character string specifying the method of estimating survival
#' curves for Cox proportional hazards-based models.  Possible values are
#' \code{"breslow"}, \code{"efron"} (default), or \code{"fleming-harrington"}.
#' @param dist character string specifying distributional approximations to
#' estimated survival curves.  Possibilities are \code{"none"} (default),
#' \code{"exponential"}, or \code{"weibull"}.
#' @param ... arguments passed to model-specific prediction functions.
#' 
#' @seealso \code{\link{fit}}, \code{\link{confusion}},
#' \code{\link{performance}}
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
predict.MLModelFit <- function(object, newdata = NULL, times = numeric(),
                               type = c("response", "prob"), cutoff = 0.5,
                               method = NULL, dist = NULL, ...) {
  newdata <- preprocess(fitbit(object, "x"), newdata)
  requireModelNamespaces(fitbit(object, "packages"))
  obs <- response(object)
  pred <- fitbit(object, "predict")(unMLModelFit(object), newdata,
                                    fitbits = field(object, "fitbits"),
                                    times = times, method = method, dist = dist,
                                    ...)
  pred <- convert_prob(obs, pred, times = times)
  if (match.arg(type) == "response") {
    pred <- convert_response(obs, pred, cutoff = cutoff)
  }
  pred
}
