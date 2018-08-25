predict.MLModelFit <- function(object, data, type = "response", cutoff = 0.5,
                               times = NULL, ...) {
  predict <- if(isS4(object)) object@.predict else object$.predict
  predict(object, data, type = type, cutoff = cutoff, times = times)
}


predict.survfit <- function(object, times, ...) {
  survtimes <- c(0, object$time)
  survprobs <- c(1, object$surv)
  idx <- sapply(times, function(x) max(which(survtimes <= x)))
  survprobs[idx]
}
