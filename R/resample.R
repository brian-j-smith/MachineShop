setGeneric("resample", function(object, x, ...) standardGeneric("resample"))


setMethod("resample", c("formula", "data.frame"),
  function(object, x, model, control) {
    resample(control, object, x, model)
  }
)


setMethod("resample", c("BootControl", "formula"),
  function(object, x, data, model) {
    datafit <- fit(x, data, model)
    obs <- eval(x[[2]], data)
    bootids <- createResample(obs, times = object@number)
    foreach(bootid = bootids, .packages = c("survival", "MLModels"),
            .combine = "rbind") %dopar% {
      pred <- predict(datafit, data[bootid,], type = "prob",
                      times = object@survtimes)
      summary(object, obs, pred)
    } %>%
      as.data.frame %>%
      structure(class = c("Resamples", "data.frame"))
  }
)


setMethod("resample", c("CVControl", "formula"),
  function(object, x, data, model) {
    foldids <- createMultiFolds(eval(x[[2]], data),
                                k = object@folds,
                                times = object@repeats)
    foreach(foldid = foldids, .packages = c("survival", "MLModels"),
            .combine = "rbind") %dopar% {
      train <- data[foldid,]
      test <- data[-foldid,]
      trainfit <- fit(x, train, model)
      obs <- eval(x[[2]], test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>%
      as.data.frame %>%
      structure(class = c("Resamples", "data.frame"))
  }
)
