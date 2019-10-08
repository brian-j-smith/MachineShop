#' Resample Estimation of Model Performance
#' 
#' Estimation of the predictive performance of a model estimated and evaluated
#' on training and test samples generated from an observed data set.
#' 
#' @name resample
#' @rdname resample-methods
#' 
#' @param x defines a relationship between model predictor and response
#'   variables.  May be a \code{\link{formula}}, design \code{\link{matrix}} of
#'   predictors, \code{\link{ModelFrame}}, untrained
#'   \code{\link[recipes]{recipe}}, or \code{\link{TunedRecipe}} object.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or call.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param ... named or unnamed \code{resample} output to combine together with
#'   the \code{Resamples} constructor.
#' 
#' @return \code{Resamples} class object.
#' 
#' @seealso \code{\link{performance}}, \code{\link{metrics}},
#' \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- Species ~ .
#' control <- CVControl()
#' 
#' gbm_res1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbm_res2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbm_res3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' 
#' summary(gbm_res1)
#' plot(gbm_res1)
#' 
#' res <- Resamples(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
#' summary(res)
#' plot(res)
#' 
resample <- function(x, ...) {
  UseMethod("resample")
}


#' @rdname resample-methods
#' 
#' @details
#' Stratified resampling is performed for the \code{formula} method according to
#' values of the response variable; i.e. categorical levels for \code{factor},
#' continuous for \code{numeric}, and event status \code{Surv}.
#' 
resample.formula <- function(x, data, model,
                             control = MachineShop::settings("control"), ...) {
  resample(ModelFrame(x, data, na.rm = FALSE,
                      strata = strata(response(x, data))), model, control)
}


#' @rdname resample-methods
#' 
resample.matrix <- function(x, y, model,
                            control = MachineShop::settings("control"), ...) {
  resample(ModelFrame(x, y, na.rm = FALSE, strata = strata(y)), model, control)
}


#' @rdname resample-methods
#' 
#' @details
#' User-specified stratification variables may be specified for
#' \code{ModelFrames} upon creation with the \code{\link[=ModelFrame]{strata}}
#' argument in its constructor.  Resampling of this class is unstratified by
#' default.
#' 
resample.ModelFrame <- function(x, model,
                                control = MachineShop::settings("control"),
                                ...) {
  .resample(getMLObject(control, "MLControl"), x, model)
}


#' @rdname resample-methods
#' 
#' @details
#' Variables in a \code{recipe} may be used for stratification by defining a
#' "case_strata" \code{\link[recipes:roles]{role}} for them.  Resampling will
#' be unstratified if no variables have that role.
#' 
resample.recipe <- function(x, model,
                            control = MachineShop::settings("control"), ...) {
  .resample(getMLObject(control, "MLControl"), ModelRecipe(x), model)
}


#' @rdname resample-methods
#' 
#' @details
#' Output being combined from more than one model with the \code{Resamples}
#' constructor must have been generated with the same resampling \code{control}
#' object.
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


setGeneric(".resample", function(object, x, ...) standardGeneric(".resample"))


setMethod(".resample", c("MLBootControl", "ModelFrame"),
  function(object, x, model) {
    presets <- MachineShop::settings()
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(x,
                         times = object@samples,
                         strata = strata) %>% rsample2caret
    index <- splits$index
    seeds <- sample.int(.Machine$integer.max, length(index))
    
    is_optimism_control <- is(object, "MLBootOptimismControl")
    if (is_optimism_control) {
      train_pred <- resample_args(x, x, model, object)[[1]]$Predicted
    }
    
    foreach(i = seq(index),
            .packages = c("MachineShop", "survival")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      if (is_optimism_control) {
        args <- resample_args(train, list(x, train), model, object, strata)
        df <- args[[1]][[1]]
        df_boot <- args[[1]][[2]]
        df$Boot.Observed <- df_boot$Observed
        df$Boot.Predicted <- df_boot$Predicted
        df$Train.Predicted <- train_pred
        args[[1]] <- df
        args
      } else {
        resample_args(train, x, model, object, strata)
      }
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLBootControl", "ModelRecipe"),
  function(object, x, model) {
    presets <- MachineShop::settings()
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(as.data.frame(x),
                         times = object@samples,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    
    test <- prep(x)
    is_optimism_control <- is(object, "MLBootOptimismControl")
    if (is_optimism_control) {
      train_pred <- resample_args(x, test, model, object)[[1]]$Predicted
    }
    
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- recipe(x, analysis(split))
      if (is_optimism_control) {
        test_list <- list(test, prep(train))
        args <- resample_args(train, test_list, model, object, strata)
        df <- args[[1]][[1]]
        df_boot <- args[[1]][[2]]
        indices <- seq_boot(df_boot, df)
        df$Boot.Observed <- df_boot$Observed[indices]
        df$Boot.Predicted <- df_boot$Predicted[indices]
        df$Train.Predicted <- train_pred
        args[[1]] <- df
        args
      } else {
        resample_args(train, test, model, object, strata)
      }
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLCVControl", "ModelFrame"),
  function(object, x, model) {
    presets <- MachineShop::settings()
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
      MachineShop::settings(presets)
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[-index[[i]], , drop = FALSE]
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLCVControl", "ModelRecipe"),
  function(object, x, model) {
    presets <- MachineShop::settings()
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- vfold_cv(as.data.frame(x),
                       v = object@folds,
                       repeats = object@repeats,
                       strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- recipe(x, analysis(split))
      test <- prep(recipe(x, assessment(split)))
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLOOBControl", "ModelFrame"),
  function(object, x, model) {
    presets <- MachineShop::settings()
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
      MachineShop::settings(presets)
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[indexOut[[i]], , drop = FALSE]
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLOOBControl", "ModelRecipe"),
  function(object, x, model) {
    presets <- MachineShop::settings()
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(as.data.frame(x),
                         times = object@samples,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))
    foreach(i = seq(splits),
            .packages = c("MachineShop", "recipes", "survival")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- recipe(x, analysis(split))
      test <- prep(recipe(x, assessment(split)))
      resample_args(train, test, model, object, strata)
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLSplitControl", "ModelFrame"),
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


setMethod(".resample", c("MLSplitControl", "ModelRecipe"),
  function(object, x, model) {
    strata <- strata_var(x)
    set.seed(object@seed)
    split <- initial_split(as.data.frame(x),
                           prop = object@prop,
                           strata = strata)
    train <- recipe(x, analysis(split))
    test <- prep(recipe(x, testing(split)))
    do.call(Resamples, resample_args(train, test, model, object, strata))
  }
)


setMethod(".resample", c("MLTrainControl", "ModelFrame"),
  function(object, x, model) {
    set.seed(object@seed)
    do.call(Resamples, resample_args(x, x, model, object))
  }
)


setMethod(".resample", c("MLTrainControl", "ModelRecipe"),
  function(object, x, model) {
    set.seed(object@seed)
    do.call(Resamples, resample_args(x, prep(x), model, object))
  }
)


resample_args <- function(train, test, model, control, strata = character()) {
  model <- getMLObject(model, "MLModel")
  
  trainfit <- fit(train, model)
  if (is(trainfit, "StackedModel")) control@times <- trainfit$times
  
  f <- function(test) {
    df <- data.frame(Model = factor(model@name),
                     Resample = 1,
                     Case = as.data.frame(test, original = FALSE)$"(casenames)",
                     stringsAsFactors = FALSE)
    df$Observed <- response(test)
    df$Predicted <- predict(trainfit, as.data.frame(test), type = "prob",
                            times = control@times, method = control@method,
                            dist = control@dist)
    df
  }
  
  list(if (class(test)[1] == "list") lapply(test, f) else f(test),
       .control = control, .strata = strata)
}


Resamples.list <- function(x) {
  resample_list <- lapply(x, function(args) args[[1]])
  resample_df <- do.call(append, resample_list)
  num_times <- sapply(resample_list, nrow)
  resample_df$Resample <- rep(seq_along(num_times), num_times)
  Resamples(resample_df, .control = x[[1]]$.control, .strata = x[[1]]$.strata)
}
