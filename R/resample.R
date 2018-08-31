setGeneric("resample", function(object, x, ...) standardGeneric("resample"))


setMethod("resample", c("MLModel", "data.frame"),
  function(object, x, control) {
    resample(control, x, object)
  }
)


setMethod("resample", c("MLModel", "formula"),
  function(object, x, data, control) {
    resample(control, model.frame(x, data), object)
  }
)


setMethod("resample", c("BootControl", "data.frame"),
  function(object, x, model) {
    obs <- model.response(x)
    bootids <- createResample(obs, times = object@number)
    foreach(bootid = bootids, .packages = c("survival", "MLModels"),
            .combine = "rbind") %dopar% {
      trainfit <- fit(model, x[bootid,])
      pred <- predict(trainfit, x, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample", c("CVControl", "data.frame"),
  function(object, x, model) {
    foldids <- createMultiFolds(model.response(x),
                                k = object@folds,
                                times = object@repeats)
    foreach(foldid = foldids, .packages = c("survival", "MLModels"),
            .combine = "rbind") %dopar% {
      train <- x[foldid,]
      test <- x[-foldid,]
      trainfit <- fit(model, train)
      obs <- model.response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)
