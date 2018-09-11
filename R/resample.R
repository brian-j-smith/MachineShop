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
#' @param ... arguments passed to other methods.
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
#' @seealso \code{\link{tune}}, \code{\link[stats]{model.frame}},
#' \code{\link[recipes]{recipe}}, \code{\link{BootControl}},
#' \code{\link{CVControl}}, \code{\link{OOBControl}}, \code{\link{Resamples}},
#' \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' ## Survival analysis example
#' library(survival)
#' 
#' (gbmperf <- resample(GBMModel(),
#'                      Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
#'                                           pat.karno + meal.cal + wt.loss,
#'                      data = lung,
#'                      control = CVControl(folds = 10, repeats = 5,
#'                                          survtimes = 365 * c(0.5, 1, 1.5))))
#' summary(gbmperf)
#' plot(gbmperf)
#' 
setMethod("resample", c("MLModel", "data.frame"),
  function(object, x, control = CVControl()) {
    .resample(control, x, object)
  }
)


#' @rdname resample-methods
#' @aliases resample,MLModel,formula-method
#' 
#' @param data data frame containing observed predictors and outcomes.
#' 
setMethod("resample", c("MLModel", "formula"),
  function(object, x, data, control = CVControl()) {
    .resample(control, model.frame(x, data), object)
  }
)


#' @rdname resample-methods
#' @aliases resample,MLModel,recipe-method
#' 
setMethod("resample", c("MLModel", "recipe"),
  function(object, x, control = CVControl()) {
    .resample(control, x, object)
  }
)


setGeneric(".resample", function(object, x, ...) {
 standardGeneric(".resample") 
})


setMethod(".resample", c("BootControl", "data.frame"),
  function(object, x, model) {
    set.seed(object@seed)
    obs <- response(x)
    splits <- bootstraps(data.frame(strata = strata(obs)),
                         times = object@number,
                         strata = "strata") %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      trainfit <- fit(model, x[index[[i]], , drop = FALSE])
      pred <- predict(trainfit, x, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% Resamples
  }
)


setMethod(".resample", c("BootControl", "recipe"),
  function(object, x, model) {
    strataname <- prep(x) %>% formula %>% terms %>% response
    set.seed(object@seed)
    splits <- bootstraps(x$template,
                         times = object@number,
                         strata = strataname)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    test <- juice(prep(x, retain = TRUE))
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
    } %>% Resamples
  }
)


setMethod(".resample", c("CVControl", "data.frame"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- vfold_cv(data.frame(strata = strata(response(x))),
                       v = object@folds,
                       repeats = object@repeats,
                       strata = "strata") %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[-index[[i]], , drop = FALSE]
      trainfit <- fit(model, train)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% Resamples
  }
)


setMethod(".resample", c("CVControl", "recipe"),
  function(object, x, model) {
    strataname <- prep(x) %>% formula %>% terms %>% response
    set.seed(object@seed)
    splits <- vfold_cv(x$template,
                       v = object@folds,
                       repeats = object@repeats,
                       strata = strataname)$splits
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
    } %>% Resamples
  }
)


setMethod(".resample", c("OOBControl", "data.frame"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- bootstraps(data.frame(strata = strata(response(x))),
                         times = object@number,
                         strata = "strata") %>% rsample2caret
    index <- splits$index
    indexOut <- splits$indexOut
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MLModels", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[indexOut[[i]], , drop = FALSE]
      if(nrow(test) == 0) return(NA)
      trainfit <- fit(model, train)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@survtimes)
      summary(object, obs, pred)
    } %>% Resamples
  }
)


setMethod(".resample", c("OOBControl", "recipe"),
  function(object, x, model) {
    strataname <- prep(x) %>% formula %>% terms %>% response
    set.seed(object@seed)
    splits <- bootstraps(x$template,
                         times = object@number,
                         strata = strataname)$splits
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
    } %>% Resamples
  }
)
