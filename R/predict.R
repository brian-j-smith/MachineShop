predict.MLModelFit <- function(object, data, type = "response", cutoff = 0.5,
                               times = NULL, ...) {
  predict <- if(isS4(object)) object@.predict else object$.predict
  predict(object, data, type = type, cutoff = cutoff, times = times)
}
