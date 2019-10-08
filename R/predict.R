#' Model Prediction
#' 
#' Predict outcomes with a fitted model.
#' 
#' @name predict
#' 
#' @param object model \link{fit} result.
#' @param newdata optional \link[=data.frame]{data frame} with which to obtain
#' predictions.  If not specified, the training data will be used by default.
#' @param times numeric vector of follow-up times at which to predict
#' survival events/probabilities or \code{NULL} for predicted survival means.
#' @param type specifies prediction on the original outcome scale
#' (\code{"response"}) or on a probability distribution scale (\code{"prob"}).
#' @param cutoff numeric (0, 1) threshold above which binary factor
#' probabilities are classified as events and below which survival probabilities
#' are classified.
#' @param dist character string specifying distributional approximations to
#' estimated survival curves.  Possible values are \code{"empirical"},
#' \code{"exponential"}, \code{"rayleigh"}, or \code{"weibull"}; with defaults
#' of \code{"empirical"} for predicted survival events/probabilities and
#' \code{"weibull"} for predicted survival means.
#' @param method character string specifying the empirical method of estimating
#' baseline survival curves for Cox proportional hazards-based models.
#' Choices are \code{"breslow"}, \code{"efron"} (default), or
#' \code{"fleming-harrington"}.
#' @param ... arguments passed to model-specific prediction functions.
#' 
#' @seealso \code{\link{confusion}}, \code{\link{performance}},
#' \code{\link{metrics}}
#' 
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' gbm_fit <- fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'                data = Melanoma, model = GBMModel)
#' predict(gbm_fit, newdata = Melanoma, times = 365 * c(2, 5, 10), type = "prob")
#' 
predict.MLModelFit <- function(object, newdata = NULL, times = NULL,
                               type = c("response", "prob"), cutoff = 0.5,
                               dist = NULL, method = NULL, ...) {
  newdata <- preprocess(fitbit(object, "x"), newdata)
  requireModelNamespaces(fitbit(object, "packages"))
  obs <- response(object)
  pred <- fitbit(object, "predict")(unMLModelFit(object), newdata,
                                    fitbits = field(object, "fitbits"),
                                    times = times, dist = dist, method = method,
                                    ...)
  pred <- convert_prob(obs, pred, times = times)
  if (match.arg(type) == "response") {
    pred <- convert_response(obs, pred, cutoff = cutoff)
  }
  pred
}
