predict.R6 <- function(class, ...) class$new()$predict(...)


predict.CForestFit <- function(object, data, times = NULL, ...) {
  predict.R6(CForestModel, object, data, times)
}


predict.CoxFit <- function(object, data, times = NULL, ...) {
  predict.R6(CoxModel, object, data, times)
}


predict.GBMFit <- function(object, data, times = NULL, ...) {
  predict.R6(GBMModel, object, data, times)
}


predict.GLMNetFit <- function(object, data, times = NULL, ...) {
  predict.R6(GLMNetModel, object, data, times)
}
