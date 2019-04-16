#' Resample Estimation of Model Performance
#' 
#' Estimation of the predictive performance of a model estimated and evaluated
#' on training and test samples generated from an observed data set.
#' 
#' @name resample
#' @rdname resample-methods
#' 
#' @param ... named or unnamed \code{resample} output to combine together with
#' the \code{Resamples} constructor.
#' 
#' @details Output being combined from more than one model with the
#' \code{Resamples} constructor must have been generated with the same
#' resampling \code{control} object.
#' 
Resamples <- function(...) {
  .Resamples(...)
}


.Resamples <- function(..., .control = NULL, .strata = NULL) {
  args <- list(...)
  
  if (length(args) == 0) stop("no resample output given")
  
  .Data <- args[[1]]
  if (all(mapply(is, args, "Resamples"))) {
    
    control <- .Data@control
    if (!all(sapply(args, function(x) identical(x@control, control)))) {
      stop("Resamples arguments have different control structures")
    }

    strata <- .Data@strata
    if (!all(sapply(args, function(x) identical(x@strata, strata)))) {
      stop("Resamples arguments have different strata variables")
    }

  } else if (length(args) > 1) {
    
    stop("arguments to combine must be Resamples objects")
    
  } else if (!is.data.frame(.Data)) {
    
    stop("Resamples argument must inherit from data.frame")
    
  } else {

    control <- .control
    if (!is(control, "MLControl")) {
      stop("missing control structure in Resamples constructor")
    }
    
    strata <- as.character(.strata)

    var_names <- c("Resample", "Case", "Observed", "Predicted")
    is_missing <- !(var_names %in% names(.Data))
    if (any(is_missing)) {
      stop("missing resample variables: ", toString(var_names[is_missing]))
    }
    
  }

  args <- make_unique_levels(args, which = "Model")
  new("Resamples", do.call(append, args), control = control, strata = strata)
}


#' @rdname resample-methods
#' 
#' @param x defines a relationship between model predictor and response
#' variables.  May be a \code{formula}, design matrix of predictors,
#' \code{ModelFrame}, or \code{recipe}.
#' 
#' @return \code{Resamples} class object.
#' 
resample <- function(x, ...) {
  UseMethod("resample")
}


#' @rdname resample-methods
#' 
#' @param data \code{data.frame} containing observed predictors and outcomes.
#' @param model \code{MLModel} object, constructor function, or character string
#' naming a constructor function that returns an \code{MLModel} object.
#' @param control \code{\link{MLControl}} object, control function, or character
#' string naming a control function defining the resampling method to be
#' employed.
#' 
#' @details
#' Stratified resampling is performed for the \code{formula} method according to
#' values of the response variable; i.e. categorical levels for \code{factor},
#' continuous for \code{numeric}, and event status \code{Surv}.
#' 
#' @seealso \code{\link{ModelFrame}}, \code{\link[recipes]{recipe}},
#' \code{\link{modelinfo}}, \code{\link{MLControl}}, \code{\link{performance}},
#' \code{\link{metricinfo}}, \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- Species ~ .
#' control <- CVControl()
#' 
#' gbmres1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbmres2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbmres3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' 
#' summary(gbmres1)
#' plot(gbmres1)
#' 
#' res <- Resamples(GBM1 = gbmres1, GBM2 = gbmres2, GBM3 = gbmres3)
#' summary(res)
#' plot(res)
#' 
resample.formula <- function(x, data, model, control = CVControl, ...) {
  resample(ModelFrame(x, data, na.rm = FALSE,
                      strata = strata(response(x, data))), model, control)
}


#' @rdname resample-methods
#' 
#' @param y predictor variable.
#' 
resample.matrix <- function(x, y, model, control = CVControl, ...) {
  resample(ModelFrame(x, y, na.rm = FALSE, strata = strata(y)), model, control)
}


#' @rdname resample-methods
#' 
#' @details
#' User-specified stratification variables may be specified for
#' \code{\link[=ModelFrame]{ModelFrames}} upon creation with the \code{strata}
#' argument in its constructor.  Resampling of this class is unstratified by
#' default.
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


setMethod(".resample", c("MLControlBoot", "ModelFrame"),
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
      resample_args(train, x, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLControlBoot", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(as.data.frame(x),
                         times = object@samples,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    test <- ModelFrame(formula(terms(x)), x)
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x)
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLControlCV", "ModelFrame"),
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
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLControlCV", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- vfold_cv(as.data.frame(x),
                       v = object@folds,
                       repeats = object@repeats,
                       strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x)
      test <- ModelFrame(formula(terms(x)), assessment(split))
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLControlOOB", "ModelFrame"),
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
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLControlOOB", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(as.data.frame(x),
                         times = object@samples,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- prepper(split, recipe = x)
      test <- ModelFrame(formula(terms(x)), assessment(split))
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLControlSplit", "ModelFrame"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    split <- initial_split(x,
                           prop = object@prop,
                           strata = strata)
    train <- x[split$in_id, , drop = FALSE]
    test <- x[-split$in_id, , drop = FALSE]
    do.call(Resamples, resample_args(train, test, model, object, strata))
  }
)


setMethod(".resample", c("MLControlSplit", "recipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    split <- initial_split(as.data.frame(x),
                           prop = object@prop,
                           strata = strata)
    train <- prepper(split, recipe = x)
    test <- ModelFrame(formula(terms(x)), testing(split))
    do.call(Resamples, resample_args(train, test, model, object, strata))
  }
)


setMethod(".resample", c("MLControlTrain", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    do.call(Resamples, resample_args(x, x, model, object))
  }
)


setMethod(".resample", c("MLControlTrain", "recipe"),
  function(object, x, model) {
    set.seed(object@seed)
    train <- prep(x)
    test <- ModelFrame(formula(terms(x)), x)
    do.call(Resamples, resample_args(train, test, model, object))
  }
)


resample_args <- function(train, test, model, control, strata = character()) {
  model <- getMLObject(model, "MLModel")
  
  trainfit <- fit(train, model)
  if (is(trainfit, "StackedModel")) control@times <- trainfit$times
  
  df <- data.frame(Model = factor(model@name),
                   Resample = 1,
                   Case = row.names(test))
  df$Observed <- response(test)
  df$Predicted <- predict(trainfit, test, type = "prob", times = control@times,
                          method = control@method, dist = control@dist)
  
  list(df, .control = control, .strata = strata)
}


Resamples.list <- function(x) {
  resample_list <- lapply(x, function(args) args[[1]])
  resample_df <- do.call(append, resample_list)
  num_times <- sapply(resample_list, nrow)
  resample_df$Resample <- rep(seq_along(num_times), num_times)
  Resamples(resample_df, .control = x[[1]]$.control, .strata = x[[1]]$.strata)
}
