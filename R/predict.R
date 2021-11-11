#' Model Prediction
#'
#' Predict outcomes with a fitted model.
#'
#' @name predict
#'
#' @param object model \link{fit} result.
#' @param newdata optional \link[=data.frame]{data frame} with which to obtain
#'   predictions.  If not specified, the training data will be used by default.
#' @param times numeric vector of follow-up times at which to predict
#'   survival events/probabilities or \code{NULL} for predicted survival means.
#' @param type specifies prediction on the original outcome scale
#'   (\code{"response"}) or on a probability distribution scale (\code{"prob"}).
#' @param cutoff numeric (0, 1) threshold above which binary factor
#'   probabilities are classified as events and below which survival
#'   probabilities are classified.
#' @param distr character string specifying distributional approximations to
#'   estimated survival curves.  Possible values are \code{"empirical"},
#'   \code{"exponential"}, \code{"rayleigh"}, or \code{"weibull"}; with defaults
#'   of \code{"empirical"} for predicted survival events/probabilities and
#'   \code{"weibull"} for predicted survival means.
#' @param method character string specifying the empirical method of estimating
#'   baseline survival curves for Cox proportional hazards-based models.
#'   Choices are \code{"breslow"} or \code{"efron"} (default).
#' @param ... arguments passed to model-specific prediction functions.
#'
#' @seealso \code{\link{confusion}}, \code{\link{performance}},
#' \code{\link{metrics}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Survival response example
#' library(survival)
#'
#' gbm_fit <- fit(Surv(time, status) ~ ., data = veteran, model = GBMModel)
#' predict(gbm_fit, newdata = veteran, times = c(90, 180, 360), type = "prob")
#' }
#'
predict.MLModelFit <- function(
  object, newdata = NULL, times = numeric(), type = c("response", "prob"),
  cutoff = MachineShop::settings("cutoff"), distr = character(),
  method = character(), ...
) {
  if (length(times)) {
    times <- check_numeric(times, bounds = c(0, Inf), include = 0, size = NA)
  }
  throw(check_assignment(times))

  model <- as.MLModel(object)
  throw(check_packages(model@packages))
  obs <- response(object)
  pred <- .predict(model, model_fit = object, newdata = newdata, times = times,
                   distr = distr, method = method, ...)
  pred <- convert_prob(obs, pred)
  if (match.arg(type) == "response") {
    convert_response(obs, pred, cutoff = cutoff)
  } else pred
}


.predict <- function(object, ...) {
  UseMethod(".predict")
}


.predict.MLModel <- function(object, model_fit, newdata, ...) {
  object@predict(
    unMLModelFit(model_fit), newdata = predictor_frame(object, newdata),
    model = object, ...
  )
}
