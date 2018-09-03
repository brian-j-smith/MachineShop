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


setMethod("resample", c("MLModel", "recipe"),
  function(object, x, control) {
    resample(control, x, object)
  }
)


setMethod("resample", c("BootControl", "data.frame"),
  function(object, x, model) {
    obs <- response(x)
    bootids <- createResample(obs, times = object@number)
    foreach(bootid = bootids,
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      trainfit <- fit(model, x[bootid,])
      pred <- predict(trainfit, x, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample", c("BootControl", "recipe"),
  function(object, x, model) {
    x_prep <- prep(x, retain = TRUE)
    bt_samples <- bootstraps(x$template,
                             times = object@number,
                             strata = x_prep %>% formula %>% terms %>% response)
    test <- juice(x_prep)
    obs <- response(formula(test), test)
    foreach(split = bt_samples$splits,
            .packages = c("MLModels", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      trainfit <- fit(model, train)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample", c("CVControl", "data.frame"),
  function(object, x, model) {
    foldids <- createMultiFolds(response(x),
                                k = object@folds,
                                times = object@repeats)
    foreach(foldid = foldids,
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      train <- x[foldid,]
      test <- x[-foldid,]
      trainfit <- fit(model, train)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample", c("CVControl", "recipe"),
  function(object, x, model) {
    cv_samples <- vfold_cv(x$template,
                           v = object@folds,
                           repeats = object@repeats,
                           strata = prep(x) %>% formula %>% terms %>% response)
    foreach(split = cv_samples$splits,
            .packages = c("MLModels", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- bake(train, newdata = assessment(split))
      trainfit <- fit(model, train)
      obs <- response(formula(train), test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)
