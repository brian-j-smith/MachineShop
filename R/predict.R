predict.CForestFit <- function(object, data, type = "response", cutoff = 0.5,
                               times = NULL, ...) {
  CForestModel()@predict(object, data, type = type, cutoff = cutoff,
                         times = times)
}


predict.CoxFit <- function(object, data, type = "response", cutoff = 0.5,
                           times = NULL, ...) {
  CoxModel()@predict(object, data, type = type, cutoff = cutoff, times = times)
}


predict.GBMFit <- function(object, data, type = "response", cutoff = 0.5,
                           times = NULL, ...) {
  GBMModel()@predict(object, data, type = type, cutoff = cutoff, times = times)
}


predict.GLMNetFit <- function(object, data, type = "response", cutoff = 0.5,
                              times = NULL, ...) {
  GLMNetModel()@predict(object, data, type = type, cutoff = cutoff,
                       times = times)
}
