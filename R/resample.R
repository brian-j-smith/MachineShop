#' Resample Estimation of Model Performance
#' 
#' Estimation of the predictive performance of a model estimated and evaluated
#' on training and test samples generated from an observed data set.
#' 
#' @name resample
#' @rdname resample-methods
#' 
#' @param x defined relationship between model predictors and an outcome.  May
#' be a ModelFrame containing a formula, data, and optionally case weights; a
#' formula; or a recipe.
#' @param ... arguments passed to other methods.
#' 
resample <- function(x, ...) {
  UseMethod("resample")
}


#' @rdname resample-methods
#' 
resample.ModelFrame <- function(x, model, control = CVControl, ...) {
  .resample(getMLObject(control, "MLControl"), x, model)
}


#' @rdname resample-methods
#' 
#' @param data data frame containing observed predictors and outcomes.
#' @param model MLModel object, constructor function, or character string
#' naming a constructor function that returns an MLModel object.
#' @param control \code{\linkS4class{MLControl}} object, control function, or
#' character string naming a control function defining the resampling method to
#' be employed.
#' 
#' @return Resamples class object.
#' 
#' @seealso \code{\link{tune}}, \code{\link{ModelFrame}},
#' \code{\link[recipes]{recipe}}, \code{\link{BootControl}},
#' \code{\link{CVControl}}, \code{\link{OOBControl}}, \code{\link{Resamples}},
#' \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' \donttest{
#' ## Survival response example
#' library(survival)
#' 
#' (gbmperf <- resample(Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
#'                                           pat.karno + meal.cal + wt.loss,
#'                      data = lung,
#'                      model = GBMModel,
#'                      control = CVControl(folds = 10, repeats = 5,
#'                                          surv_times = c(180, 360, 540))))
#' summary(gbmperf)
#' plot(gbmperf)
#' }
#' 
resample.formula <- function(x, data, model, control = CVControl, ...) {
  resample(ModelFrame(x, data, na.action = na.pass), model, control)
}


#' @rdname resample-methods
#' 
resample.recipe <- function(x, model, control = CVControl, ...) {
  .resample(getMLObject(control, "MLControl"), x, model)
}


setGeneric(".resample", function(object, x, ...) standardGeneric(".resample"))


setMethod(".resample", c("BootControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    obs <- response(x)
    splits <- bootstraps(data.frame(strata = strata(obs)),
                         times = object@samples,
                         strata = "strata") %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      trainfit <- fit(x[index[[i]], , drop = FALSE], model)
      pred <- predict(trainfit, x, type = "prob", times = object@surv_times)
      summary(object, obs, pred)
    } %>% Resamples(method = method(object), seed = object@seed)
  }
)


setMethod(".resample", c("BootControl", "recipe"),
  function(object, x, model) {
    strataname <- prep(x) %>% formula %>% terms %>% response
    set.seed(object@seed)
    splits <- bootstraps(x$template,
                         times = object@samples,
                         strata = strataname)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    test <- juice(prep(x, retain = TRUE))
    obs <- response(formula(test), test)
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      trainfit <- fit(train, model)
      pred <- predict(trainfit, test, type = "prob", times = object@surv_times)
      summary(object, obs, pred)
    } %>% Resamples(method = method(object), seed = object@seed)
  }
)


setMethod(".resample", c("CVControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- vfold_cv(data.frame(strata = strata(response(x))),
                       v = object@folds,
                       repeats = object@repeats,
                       strata = "strata") %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[-index[[i]], , drop = FALSE]
      trainfit <- fit(train, model)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@surv_times)
      summary(object, obs, pred)
    } %>% Resamples(method = method(object), seed = object@seed)
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
            .packages = c("MachineShop", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- bake(train, newdata = assessment(split))
      trainfit <- fit(train, model)
      obs <- response(formula(train), test)
      pred <- predict(trainfit, test, type = "prob", times = object@surv_times)
      summary(object, obs, pred)
    } %>% Resamples(method = method(object), seed = object@seed)
  }
)


setMethod(".resample", c("OOBControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- bootstraps(data.frame(strata = strata(response(x))),
                         times = object@samples,
                         strata = "strata") %>% rsample2caret
    index <- splits$index
    indexOut <- splits$indexOut
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[indexOut[[i]], , drop = FALSE]
      if (nrow(test) == 0) return(NA)
      trainfit <- fit(train, model)
      obs <- response(test)
      pred <- predict(trainfit, test, type = "prob", times = object@surv_times)
      summary(object, obs, pred)
    } %>% Resamples(method = method(object), seed = object@seed)
  }
)


setMethod(".resample", c("OOBControl", "recipe"),
  function(object, x, model) {
    strataname <- prep(x) %>% formula %>% terms %>% response
    set.seed(object@seed)
    splits <- bootstraps(x$template,
                         times = object@samples,
                         strata = strataname)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival"),
            .combine = "rbind") %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- bake(train, newdata = assessment(split))
      if (nrow(test) == 0) return(NA)
      trainfit <- fit(train, model)
      obs <- response(formula(train), test)
      pred <- predict(trainfit, test, type = "prob", times = object@surv_times)
      summary(object, obs, pred)
    } %>% Resamples(method = method(object), seed = object@seed)
  }
)
