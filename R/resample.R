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
#' @param data data frame containing observed predictors and outcomes.
#' @param model MLModel object, constructor function, or character string
#' naming a constructor function that returns an MLModel object.
#' @param control \code{\linkS4class{MLControl}} object, control function, or
#' character string naming a control function defining the resampling method to
#' be employed.
#' 
#' @details
#' Stratified resampling is performed for the \code{formula} method according to
#' values of the response variable; i.e. categorical levels for \code{factor},
#' continuous for \code{numeric}, and event status \code{Surv}.
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
#' library(MASS)
#' 
#' (gbmperf <- resample(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'                      data = Melanoma, model = GBMModel,
#'                      control = CVControl(folds = 10, repeats = 5,
#'                                          surv_times = 365 * c(2, 5, 10))))
#' summary(gbmperf)
#' plot(gbmperf)
#' }
#' 
resample.formula <- function(x, data, model, control = CVControl, ...) {
  resample(ModelFrame(x, data, strata = strata(response(x, data)),
                      na.action = na.pass), model, control)
}


#' @rdname resample-methods
#' 
#' @details
#' User-specified stratification variables may be specified for
#' \code{\link[MachineShop:ModelFrame-methods]{ModelFrames}} upon
#' creation with the \code{strata} argument in its constructor.  Resampling of
#' this class is unstratified by default.
#' 
resample.ModelFrame <- function(x, model, control = CVControl, ...) {
  .resample(getMLObject(control, "MLControl"), x, model)
}


#' @rdname resample-methods
#' 
#' @details
#' Variables in a \code{recipe} may be used for stratification by defining a
#' "case_strata" \code{\link[recipes:roles]{role}} for them.  Resampling will
#' be unstratified if no variables have that role.
#' 
resample.recipe <- function(x, model, control = CVControl, ...) {
  .resample(getMLObject(control, "MLControl"), x, model)
}


setGeneric(".resample", function(object, x, ...) standardGeneric(".resample"))


setMethod(".resample", c("BootMLControl", "ModelFrame"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(x,
                         times = object@samples,
                         strata = strata) %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival")) %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      resample_metrics(train, x, model, object)
    } %>% Resamples.list(control = object, strata = strata)
  }
)


setMethod(".resample", c("BootMLControl", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(getdata(x),
                         times = object@samples,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    test <- ModelFrame(formula(terms(x)), getdata(x))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object, strata = strata)
  }
)


setMethod(".resample", c("CVMLControl", "ModelFrame"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- vfold_cv(x,
                       v = object@folds,
                       repeats = object@repeats,
                       strata = strata) %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival")) %dopar% {
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[-index[[i]], , drop = FALSE]
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object, strata = strata)
  }
)


setMethod(".resample", c("CVMLControl", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- vfold_cv(getdata(x),
                       v = object@folds,
                       repeats = object@repeats,
                       strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- ModelFrame(formula(terms(x)), assessment(split))
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object, strata = strata)
  }
)


setMethod(".resample", c("OOBMLControl", "ModelFrame"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(x,
                         times = object@samples,
                         strata = strata) %>% rsample2caret
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
    } %>% Resamples.list(control = object, strata = strata)
  }
)


setMethod(".resample", c("OOBMLControl", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(getdata(x),
                         times = object@samples,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
      test <- ModelFrame(formula(terms(x)), assessment(split))
      if (nrow(test) == 0) return(NA)
      resample_metrics(train, test, model, object)
    } %>% Resamples.list(control = object, strata = strata)
  }
)


setMethod(".resample", c("SplitMLControl", "ModelFrame"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    split <- initial_split(x,
                           prop = object@prop,
                           strata = strata)
    train <- x[split$in_id, , drop = FALSE]
    test <- x[-split$in_id, , drop = FALSE]
    perf <- resample_metrics(train, test, model, object)
    Resamples(perf$metrics, response = cbind(Resample = 1, perf$response),
              control = object, strata = strata)
  }
)


setMethod(".resample", c("SplitMLControl", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    split <- initial_split(getdata(x),
                           prop = object@prop,
                           strata = strata)
    train <- prepper(split, recipe = x, retain = TRUE, verbose = FALSE)
    test <- ModelFrame(formula(terms(x)), testing(split))
    perf <- resample_metrics(train, test, model, object)
    Resamples(perf$metrics, response = cbind(Resample = 1, perf$response),
              control = object, strata = strata)
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
    test <- ModelFrame(formula(terms(x)), getdata(x))
    perf <- resample_metrics(x, test, model, object)
    Resamples(perf$metrics, response = cbind(Resample = 1, perf$response),
              control = object)
  }
)


resample_metrics <- function(train, test, model, control) {
  model <- getMLObject(model, "MLModel")
  
  trainfit <- fit(train, model)
  obs <- response(test)
  pred <- predict(trainfit, test, type = "prob", times = control@surv_times)
  
  response  <- data.frame(Case = row.names(test))
  response$Observed <- obs
  response$Predicted <- pred
  response$Model <- factor(model@name)
  
  if (is(trainfit, "StackedModel")) control@surv_times <- trainfit$times
  
  list(metrics = rbind(summary(control, obs, pred)), response = response)
}


Resamples.list <- function(x, control, strata) {
  metrics <- Reduce(append, lapply(x, getElement, name = "metrics"))
  rownames(metrics) <- seq(x)
  
  response_list <- lapply(seq(x), function(i) {
    cbind(Resample = i, x[[i]]$response)
  })
  response <- Reduce(append, response_list)

  Resamples(metrics, control = control, response = response, strata = strata)
}
