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
    splits <- createResample(obs, times = object@number)
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      trainfit <- fit(model, x[split, , drop = FALSE])
      pred <- predict(trainfit, x, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample_sub", c("BootControl", "recipe"),
  function(object, x, model) {
    x_prep <- prep(x, retain = TRUE)
    strata <- x_prep %>% formula %>% terms %>% response
    splits <- bootstraps(x$template,
                         times = object@number,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    test <- juice(x_prep)
    obs <- response(formula(test), test)
    foreach(i = seq(splits),
            .packages = c("MLModels", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      trainfit <- fit(model, train)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample_sub", c("CVControl", "data.frame"),
  function(object, x, model) {
    splits <- createMultiFolds(response(x),
                               k = object@folds,
                               times = object@repeats)
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- x[split, , drop = FALSE]
      test <- x[-split, , drop = FALSE]
      trainfit <- fit(model, train)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% as("Resamples")
  }
)


setMethod("resample_sub", c("CVControl", "recipe"),
  function(object, x, model) {
    strata <- prep(x) %>% formula %>% terms %>% response
    splits <- vfold_cv(x$template,
                       v = object@folds,
                       repeats = object@repeats,
                       strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MLModels", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
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
    splits <- createResample(response(x), times = object@number)
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- x[split, , drop = FALSE]
      test <- x[setdiff(1:nrow(x), split), , drop = FALSE]
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
    strata <- prep(x) %>% formula %>% terms %>% response
    splits <- bootstraps(x$template,
                         times = object@number,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MLModels", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
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
