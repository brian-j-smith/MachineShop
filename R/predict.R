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
  obs <- response(object)
  pred <- vectorize(obs, predict(object, newdata, times = times))
  if (match.arg(type) == "response") {
    pred <- convert(obs, pred, cutoff = cutoff)
  }
  pred
}


setGeneric("vectorize", function(object, x) standardGeneric("vectorize"))


setMethod("vectorize", c("ANY", "ANY"),
  function(object, x) x
)


setMethod("vectorize", c("factor", "array"),
  function(object, x) {
    vectorize(object, adrop(x, length(dim(x))))
  }
)


setMethod("vectorize", c("factor", "matrix"),
  function(object, x) {
    if (nlevels(object) == 2) x[, ncol(x)] else x
  }
)


setMethod("vectorize", c("numeric", "array"),
  function(object, x) {
    vectorize(object, adrop(x, length(dim(x))))
  }
)


setMethod("vectorize", c("numeric", "matrix"),
  function(object, x) {
    stopifnot(ncol(x) == 1)
    drop(x)
  }
)


predict.survfit <- function(object, times, ...) {
  survtimes <- c(0, object$time)
  survprobs <- c(1, object$surv)
  idx <- sapply(times, function(x) max(which(survtimes <= x)))
  survprobs[idx]
}
