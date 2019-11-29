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
#'   predictors, \code{\link{ModelFrame}}, \code{\link{SelectedModelFrame}},
#'   untrained \code{\link[recipes]{recipe}}, \code{\link{SelectedRecipe}}, or
#'   \code{\link{TunedRecipe}} object.  Alternatively, a \link[=models]{model}
#'   function or call may be given first followed by objects defining the
#'   predictor and response relationship and the \code{control} value.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or call.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param ... arguments passed to other methods.
#'
#' @return \code{Resamples} class object.
#'
#' @seealso \code{\link{c}}, \code{\link{metrics}}, \code{\link{performance}},
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
#' res <- c(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
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
#' "case_stratum" \code{\link[recipes:roles]{role}} for them.  Resampling will
#' be unstratified if no variables have that role.
#'
resample.recipe <- function(x, model,
                            control = MachineShop::settings("control"), ...) {
  .resample(getMLObject(control, "MLControl"), ModelRecipe(x), model)
}


#' @rdname resample-methods
#'
resample.MLModel <- function(x, ...) {
  resample(..., model = x)
}


#' @rdname resample-methods
#'
resample.MLModelFunction <- function(x, ...) {
  resample(..., model = x)
}


Resamples <- function(object, strata = NULL, ...) {
  var_names <- c("Model", "Resample", "Case", "Observed", "Predicted")
  found <- var_names %in% names(object)
  if (!all(found)) {
    subnames <- var_names[!found]
    stop(plural_suffix("missing resample variable", subnames), ": ",
         toString(subnames))
  }
  rownames(object) <- NULL
  new("Resamples", object, strata = as.character(strata), ...)
}


setGeneric(".resample", function(object, x, ...) standardGeneric(".resample"))


setMethod(".resample", c("MLBootstrapControl", "ModelFrame"),
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
            .packages = MachineShop::settings("require")) %dopar% {
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


setMethod(".resample", c("MLBootstrapControl", "ModelRecipe"),
  function(object, x, model) {
    presets <- MachineShop::settings()
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- bootstraps(as.data.frame(x),
                         times = object@samples,
                         strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))

    is_optimism_control <- is(object, "MLBootOptimismControl")
    if (is_optimism_control) {
      train_pred <- resample_args(x, x, model, object)[[1]]$Predicted
    }

    foreach(i = seq(splits),
            .packages = MachineShop::settings("require")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- recipe(x, analysis(split))
      if (is_optimism_control) {
        args <- resample_args(train, list(x, train), model, object, strata)
        df <- args[[1]][[1]]
        df_boot <- args[[1]][[2]]
        indices <- seq_boot(df_boot, df)
        df["Boot.Observed"] <- df_boot[indices, "Observed"]
        df["Boot.Predicted"] <- df_boot[indices, "Predicted"]
        df$Train.Predicted <- train_pred
        args[[1]] <- df
        args
      } else {
        resample_args(train, x, model, object, strata)
      }
    } %>% Resamples.list
  }
)


setMethod(".resample", c("MLCrossValidationControl", "ModelFrame"),
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

    is_optimism_control <- is(object, "MLCVOptimismControl")

    args_list <- foreach(i = seq(index),
                         .packages = MachineShop::settings("require")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      train <- x[index[[i]], , drop = FALSE]
      test <- x[-index[[i]], , drop = FALSE]
      if (is_optimism_control) {
        args <- resample_args(train, list(test, x), model, object, strata)
        args$CV.Predicted <- args[[1]][[2]]["Predicted"]
        args[[1]] <- args[[1]][[1]]
        args
      } else {
        resample_args(train, test, model, object, strata)
      }
    }
    res <- Resamples.list(args_list)

    if (is_optimism_control) {
      pred_list <- lapply(args_list, getElement, name = "CV.Predicted")
      split_factor <- rep(seq_len(object@folds), times = object@repeats)
      df <- split(seq(pred_list), split_factor) %>%
        lapply(function(indices) do.call(append, pred_list[indices])) %>%
        as.data.frame
      names(df) <- paste0("CV.Predicted.", seq(df))
      pred <- resample_args(x, x, model, object)[[1]]$Predicted
      df$Train.Predicted <- do.call(append, rep(list(pred), object@repeats))
      res[names(df)] <- df
    }

    res
  }
)


setMethod(".resample", c("MLCrossValidationControl", "ModelRecipe"),
  function(object, x, model) {
    presets <- MachineShop::settings()
    strata <- strata_var(x)
    set.seed(object@seed)
    splits <- vfold_cv(as.data.frame(x),
                       v = object@folds,
                       repeats = object@repeats,
                       strata = strata)$splits
    seeds <- sample.int(.Machine$integer.max, length(splits))

    is_optimism_control <- is(object, "MLCVOptimismControl")

    args_list <- foreach(i = seq(splits),
                         .packages = MachineShop::settings("require")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- recipe(x, analysis(split))
      test <- assessment(split)
      if (is_optimism_control) {
        args <- resample_args(train, list(test, x), model, object, strata)
        args$CV.Predicted <- args[[1]][[2]]["Predicted"]
        args[[1]] <- args[[1]][[1]]
        args
      } else {
        resample_args(train, test, model, object, strata)
      }
    }
    res <- Resamples.list(args_list)

    if (is_optimism_control) {
      pred_list <- lapply(args_list, getElement, name = "CV.Predicted")
      split_factor <- rep(seq_len(object@folds), times = object@repeats)
      df <- split(seq(pred_list), split_factor) %>%
        lapply(function(indices) do.call(append, pred_list[indices])) %>%
        as.data.frame
      names(df) <- paste0("CV.Predicted.", seq(df))
      pred <- resample_args(x, x, model, object)[[1]]$Predicted
      df$Train.Predicted <- do.call(append, rep(list(pred), object@repeats))
      res[names(df)] <- df
    }

    res
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
            .packages = MachineShop::settings("require")) %dopar% {
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
            .packages = MachineShop::settings("require")) %dopar% {
      MachineShop::settings(presets)
      set.seed(seeds[i])
      split <- splits[[i]]
      train <- recipe(x, analysis(split))
      test <- assessment(split)
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
    test <- testing(split)
    do.call(Resamples, resample_args(train, test, model, object, strata))
  }
)


setMethod(".resample", c("MLTrainControl", "ANY"),
  function(object, x, model) {
    set.seed(object@seed)
    do.call(Resamples, resample_args(x, x, model, object))
  }
)


resample_args <- function(train, test, model, control, strata = character()) {
  model <- getMLObject(model, "MLModel")

  trainfit <- fit(train, model)
  if (is(trainfit, "StackedModel")) control@times <- trainfit$times

  f <- function(test) {
    if (is(train, "ModelRecipe")) {
      test <- recipe(as.MLModel(trainfit)@x, as.data.frame(test))
    }
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
       control = control, strata = strata)
}


Resamples.list <- function(x) {
  resample_list <- lapply(x, function(args) args[[1]])
  resample_df <- do.call(append, resample_list)
  num_times <- sapply(resample_list, nrow)
  resample_df$Resample <- rep(seq_along(num_times), num_times)
  Resamples(resample_df, control = x[[1]]$control, strata = x[[1]]$strata)
}
