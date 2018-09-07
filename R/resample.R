#' Resample Estimation of Model Performance
#' 
#' Estimation of the predictive performance of a model estimated and evaluated
#' on training and test samples generated from an observed data set.
#' 
#' @name resample
#' @rdname resample-methods
#' 
#' @param object prediction model object.
#' @param x defined relationship between model predictors and an outcome.  May
#' be a model.frame (data.frame) containing a formula, data, and optionally case
#' weights; a formula; or a recipe.
#' @param ... further arguments passed to other methods.
#' 
setGeneric("resample", function(object, x, ...) standardGeneric("resample"))


#' @rdname resample-methods
#' @aliases resample,MLModel,data.frame-method
#' 
#' @param control \code{\linkS4class{MLControl}} object defining and controlling
#' the resampling method to be employed.
#' 
#' @return Resamples class object.
#' 
#' @seealso \code{\link[stats]{model.frame}}, \code{\link[recipes]{recipe}},
#' \code{\link{BootControl}}, \code{\link{CVControl}}, \code{\link{OOBControl}},
#' \code{\link{summary}}
#' 
setMethod("resample", c("MLModel", "data.frame"),
  function(object, x, control) {
    resample_sub(control, x, object)
  }
)


#' @rdname resample-methods
#' @aliases resample,MLModel,formula-method
#' 
#' @param data data frame containing observed predictors and outcomes.
#' 
setMethod("resample", c("MLModel", "formula"),
  function(object, x, data, control) {
    resample_sub(control, model.frame(x, data), object)
  }
)


#' @rdname resample-methods
#' @aliases resample,MLModel,recipe-method
#' 
setMethod("resample", c("MLModel", "recipe"),
  function(object, x, control) {
    resample_sub(control, x, object)
  }
)


setGeneric("resample_sub", function(object, x, ...) {
 standardGeneric("resample_sub") 
})


setMethod("resample_sub", c("BootControl", "data.frame"),
  function(object, x, model) {
    obs <- response(x)
    bootids <- createResample(obs, times = object@number)
    foreach(bootid = bootids,
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      trainfit <- fit(model, x[bootid, , drop = FALSE])
      pred <- predict(trainfit, x, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample_sub", c("BootControl", "recipe"),
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


setMethod("resample_sub", c("CVControl", "data.frame"),
  function(object, x, model) {
    foldids <- createMultiFolds(response(x),
                                k = object@folds,
                                times = object@repeats)
    foreach(foldid = foldids,
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      train <- x[foldid, , drop = FALSE]
      test <- x[-foldid, , drop = FALSE]
      trainfit <- fit(model, train)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample_sub", c("CVControl", "recipe"),
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


setMethod("resample_sub", c("OOBControl", "data.frame"),
  function(object, x, model) {
    bootids <- createResample(response(x), times = object@number)
    foreach(bootid = bootids,
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      train <- x[bootid, , drop = FALSE]
      test <- x[setdiff(1:nrow(x), bootid), , drop = FALSE]
      if(nrow(test) == 0) return(NA)
      trainfit <- fit(model, train)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample_sub", c("OOBControl", "recipe"),
  function(object, x, model) {
    bt_samples <- bootstraps(x$template,
                             times = object@number,
                             strata = prep(x) %>% formula %>% terms %>% response)
    foreach(split = bt_samples$splits,
            .packages = c("MLModels", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- bake(train, newdata = assessment(split))
      if(nrow(test) == 0) return(NA)
      trainfit <- fit(model, train)
      obs <- response(formula(train), test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)
