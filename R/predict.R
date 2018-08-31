predict.MLModelFit <- function(object, newdata, type = c("response", "prob"),
                               cutoff = 0.5, times = NULL, ...) {
  obs <- response(object)
  predict <- if(isS4(object)) object@.predict else object$.predict
  pred <- predict(object, newdata, times = times)
  if(type == "response") convert(obs, pred, cutoff = cutoff) else pred
}


predict.survfit <- function(object, times, ...) {
  survtimes <- c(0, object$time)
  survprobs <- c(1, object$surv)
  idx <- sapply(times, function(x) max(which(survtimes <= x)))
  survprobs[idx]
}
