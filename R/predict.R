predict.MLModelFit <- function(object, newdata, type = c("response", "prob"),
                               cutoff = 0.5, times = NULL, ...) {
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
