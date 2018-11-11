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


setMethod(".resample", c("BootMLControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- bootstraps(data.frame(strata = strata(response(x))),
                         times = object@samples,
                         strata = "strata") %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival")) %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      resample_metrics(train, x, model, object)
    } %>% Resamples.list(control = object)
  }
)


setMethod(".resample", c("BootMLControl", "recipe"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- bootstraps(x$template,
                         times = object@samples,
                         strata =  response(terms(x)))$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    test <- ModelFrame(formula(terms(x)), x$template)
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object)
  }
)


setMethod(".resample", c("CVMLControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- vfold_cv(data.frame(strata = strata(response(x))),
                       v = object@folds,
                       repeats = object@repeats,
                       strata = "strata") %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival")) %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[-index[[i]], , drop = FALSE]
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object)
  }
)


setMethod(".resample", c("CVMLControl", "recipe"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- vfold_cv(x$template,
                       v = object@folds,
                       repeats = object@repeats,
                       strata =  response(terms(x)))$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- ModelFrame(formula(terms(x)), assessment(split))
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object)
  }
)


setMethod(".resample", c("OOBMLControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- bootstraps(data.frame(strata = strata(response(x))),
                         times = object@samples,
                         strata = "strata") %>% rsample2caret
    index <- splits$index
    indexOut <- splits$indexOut
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival")) %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[indexOut[[i]], , drop = FALSE]
      if (nrow(test) == 0) return(NA)
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object)
  }
)


setMethod(".resample", c("OOBMLControl", "recipe"),
  function(object, x, model) {
    set.seed(object@seed)
    splits <- bootstraps(x$template,
                         times = object@samples,
                         strata =  response(terms(x)))$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- ModelFrame(formula(terms(x)), assessment(split))
      if (nrow(test) == 0) return(NA)
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object)
  }
)


setMethod(".resample", c("SplitMLControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    split <- initial_split(data.frame(strata = strata(response(x))),
                           prop = object@prop,
                           strata = "strata")
    train <- x[split$in_id, , drop = FALSE]
    test <- x[-split$in_id, , drop = FALSE]
    perf <- resample_metrics(train, test, model, object)
    Resamples(perf$metrics, response = cbind(Resample = 1, perf$response),
              control = object)
  }
)


setMethod(".resample", c("SplitMLControl", "recipe"),
  function(object, x, model) {
    set.seed(object@seed)
    split <- initial_split(x$template,
                           prop = object@prop,
                           strata =  response(terms(x)))
    train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
    test <- ModelFrame(formula(terms(x)), testing(split))
    perf <- resample_metrics(train, test, model, object)
    Resamples(perf$metrics, response = cbind(Resample = 1, perf$response),
              control = object)
  }
)


setMethod(".resample", c("TrainMLControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    perf <- resample_metrics(x, x, model, object)
    Resamples(perf$metrics, response = cbind(Resample = 1, perf$response),
              control = object)
  }
)


setMethod(".resample", c("TrainMLControl", "recipe"),
  function(object, x, model) {
    set.seed(object@seed)
    test <- ModelFrame(formula(terms(x)), x$template)
    perf <- resample_metrics(x, test, model, object)
    Resamples(perf$metrics, response = cbind(Resample = 1, perf$response),
              control = object)
  }
)


resample_metrics <- function(train, test, model, control) {
  trainfit <- fit(train, model)
  obs <- response(test)
  pred <- predict(trainfit, test, type = "prob", times = control@surv_times)
  
  response  <- data.frame(Case = row.names(test))
  response[["Observed"]] <- obs
  response[["Predicted"]] <- pred
  
  list(metrics = rbind(summary(control, obs, pred)), response = response)
}


Resamples.list <- function(x, control) {
  metrics <- Reduce(append, lapply(x, getElement, name = "metrics"))
  rownames(metrics) <- seq(x)
  
  response_list <- lapply(seq(x), function(i) {
    cbind(Resample = i, x[[i]]$response)
  })
  response <- Reduce(append, response_list)

  Resamples(metrics, control = control, response = response)
}
