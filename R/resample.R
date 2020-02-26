#' Resample Estimation of Model Performance
#'
#' Estimation of the predictive performance of a model estimated and evaluated
#' on training and test samples generated from an observed data set.
#'
#' @name resample
#' @rdname resample-methods
#'
#' @param x \link[=inputs]{input} specifying a relationship between model
#'   predictor and response variables.  Alternatively, a \link[=models]{model}
#'   function or call may be given first followed by the input specification and
#'   \code{control} value.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or call; ignored
#'   and can be omitted when resampling \link[=ModeledInput]{modeled inputs}.
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
  if (missing(model)) model <- NullModel
  .resample(getMLObject(control, "MLControl"), x, model)
}


#' @rdname resample-methods
#'
#' @details
#' Variables in \code{recipe} specifications may be designated as case strata
#' with the \code{\link{role_case}} function.  Resampling will be unstratified
#' otherwise.
#'
resample.recipe <- function(x, model,
                            control = MachineShop::settings("control"), ...) {
  if (missing(model)) model <- NullModel
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
  resample(x(), ...)
}


Resamples <- function(object, ...) {
  UseMethod("Resamples")
}


Resamples.data.frame <- function(object, ..., strata = NULL, .check = TRUE) {
  if (.check) {
    varnames <- c("Model", "Resample", "Case", "Observed", "Predicted")
    missing <- missing_names(varnames, object)
    if (length(missing)) {
      stop(label_items("missing resample variable", missing))
    }
    object$Model <- droplevels(object$Model)
  }
  rownames(object) <- NULL
  new("Resamples", object, strata = as.character(strata), ...)
}


Resamples.list <- function(object, ...) {
  Resamples(do.call(append, object), ...)
}


.resample <- function(object, x, model, ...) {
  UseMethod(".resample")
}


.resample.MLBootstrapControl <- function(object, x, model, ...) {
  presets <- MachineShop::settings()
  strata <- strata_var(x)
  set.seed(object@seed)
  splits <- bootstraps(rsample_data(x),
                       times = object@samples,
                       strata = strata)$splits
  seeds <- sample.int(.Machine$integer.max, length(splits))

  is_optimism_control <- is(object, "MLBootOptimismControl")
  if (is_optimism_control) {
    train_pred <- subsample(x, x, model, object)$Predicted
  }

  foreach(i = seq(splits),
          .packages = MachineShop::settings("require")) %dopar% {
    MachineShop::settings(presets)
    set.seed(seeds[i])
    train <- analysis(splits[[i]], x)
    if (is_optimism_control) {
      subs <- subsample(train, list(x, train), model, object, i)
      df <- subs[[1]]
      df_boot <- subs[[2]]
      indices <- seq_boot(df_boot, df)
      df["Boot.Observed"] <- df_boot[indices, "Observed"]
      df["Boot.Predicted"] <- df_boot[indices, "Predicted"]
      df$Train.Predicted <- train_pred
      df
    } else {
      subsample(train, x, model, object, i)
    }
  } %>% Resamples(control = object, strata = strata)
}


.resample.MLCrossValidationControl <- function(object, x, model, ...) {
  presets <- MachineShop::settings()
  strata <- strata_var(x)
  set.seed(object@seed)
  splits <- vfold_cv(rsample_data(x),
                     v = object@folds,
                     repeats = object@repeats,
                     strata = strata)$splits
  seeds <- sample.int(.Machine$integer.max, length(splits))

  is_optimism_control <- is(object, "MLCVOptimismControl")

  df_list <- foreach(i = seq(splits),
                     .packages = MachineShop::settings("require")) %dopar% {
    MachineShop::settings(presets)
    set.seed(seeds[i])
    train <- analysis(splits[[i]], x)
    test <- assessment(splits[[i]], x)
    if (is_optimism_control) {
      subs <- subsample(train, list(test, x), model, object, i)
      structure(subs[[1]], CV.Predicted = subs[[2]]["Predicted"])
    } else {
      subsample(train, test, model, object, i)
    }
  }
  res <- Resamples(df_list, control = object, strata = strata)

  if (is_optimism_control) {
    pred_list <- map(attr, df_list, "CV.Predicted")
    split_factor <- rep(seq_len(object@folds), times = object@repeats)
    df <- split(seq(pred_list), split_factor) %>%
      map(function(indices) do.call(append, pred_list[indices]), .) %>%
      as.data.frame
    names(df) <- paste0("CV.Predicted.", seq(df))
    pred <- subsample(x, x, model, object)$Predicted
    df$Train.Predicted <- do.call(append, rep(list(pred), object@repeats))
    res[names(df)] <- df
  }

  res
}


.resample.MLOOBControl <- function(object, x, model, ...) {
  presets <- MachineShop::settings()
  strata <- strata_var(x)
  set.seed(object@seed)
  splits <- bootstraps(rsample_data(x),
                       times = object@samples,
                       strata = strata)$splits
  seeds <- sample.int(.Machine$integer.max, length(splits))
  foreach(i = seq(splits),
          .packages = MachineShop::settings("require")) %dopar% {
    MachineShop::settings(presets)
    set.seed(seeds[i])
    train <- analysis(splits[[i]], x)
    test <- assessment(splits[[i]], x)
    subsample(train, test, model, object, i)
  } %>% Resamples(control = object, strata = strata)
}


.resample.MLSplitControl <- function(object, x, model, ...) {
  strata <- strata_var(x)
  set.seed(object@seed)
  split <- initial_split(rsample_data(x),
                         prop = object@prop,
                         strata = strata)
  train <- training(split, x)
  test <- testing(split, x)
  subsample(train, test, model, object) %>%
    Resamples(control = object, strata = strata)
}


.resample.MLTrainControl <- function(object, x, model, ...) {
  set.seed(object@seed)
  Resamples(subsample(x, x, model, object), control = object)
}


#################### Utility Functions ####################


rsample_data <- function(x, ...) UseMethod("rsample_data")
rsample_data.ModelFrame <- function(x, ...) asS3(x)
rsample_data.ModelRecipe <- function(x, ...) as.data.frame(x)


analysis <- function(x, object, ...) UseMethod("analysis", object)

analysis.ModelFrame <- function(x, object, ...) {
  as(rsample::analysis(x), class(object)[1])
}

analysis.ModelRecipe <- function(x, object, ...) {
  recipe(object, rsample::analysis(x))
}


assessment <- function(x, object, ...) UseMethod("assessment", object)

assessment.ModelFrame <- function(x, object, ...) {
  as(rsample::assessment(x), class(object)[1])
}

assessment.ModelRecipe <- function(x, object, ...) {
  rsample::assessment(x)
}


testing <- function(x, object, ...) UseMethod("testing", object)

testing.ModelFrame <- function(x, object, ...) {
  as(rsample::testing(x), class(object)[1])
}

testing.ModelRecipe <- function(x, object, ...) {
  recipe(object, rsample::testing(x))
}


training <- function(x, object, ...) UseMethod("training", object)

training.ModelFrame <- function(x, object, ...) {
  as(rsample::training(x), class(object)[1])
}

training.ModelRecipe <- function(x, object, ...) {
  recipe(object, rsample::training(x))
}


subsample <- function(train, test, model, control, id = 1) {
  model <- getMLObject(model, "MLModel")

  trainfit <- fit(train, model)
  if (is(trainfit, "StackedModel")) control@times <- trainfit$times

  f <- function(test) {
    if (is(train, "ModelRecipe")) {
      test <- recipe(as.MLModel(trainfit)@x, as.data.frame(test))
    }
    df <- data.frame(Model = factor(model@name),
                     Resample = as.integer(id),
                     Case = as.data.frame(test, original = FALSE)[["(names)"]],
                     stringsAsFactors = FALSE)
    df$Observed <- response(test)
    df$Predicted <- predict(trainfit, as.data.frame(test), type = "prob",
                            times = control@times, method = control@method,
                            dist = control@dist)
    df
  }

  if (class(test)[1] == "list") map(f, test) else f(test)
}


resample_selection <- function(x, transform, params, ...) {

  metrics <- params$metrics
  stat <- fget(params$stat)

  perf_list <- list()
  perf_stats <- numeric()
  for (name in names(x)) {
    res <- try(
      resample(transform(x[[name]]), ..., control = params$control),
      silent = TRUE
    )

    if (is(res, "try-error")) {
      warn("resampling failed for ", name, " with error:\n",
           attr(res, "condition")$message)
      perf_list[[name]] <- NA
      perf_stats[name] <- NA
      next
    }

    if (is.null(metrics)) {
      method <- fget(findS3Method(performance, res$Observed))
      metrics <- c(eval(formals(method)$metrics))
      is_defined <- map_logi(function(metric) {
        types <- metricinfo(metric)[[1]]$response_types
        any(map_logi(is, list(res$Observed), types$observed) &
              map_logi(is, list(res$Predicted), types$predicted))
      }, metrics)
      metrics <- metrics[is_defined]
    }

    perf <- performance(res, metrics = metrics, cutoff = params$cutoff)
    perf_list[[name]] <- perf
    perf_stats[name] <- stat(na.omit(perf[, 1]))
  }

  failed <- is.na(perf_list)
  if (all(failed)) {
    stop("resampling failed for all models", call. = FALSE)
  } else if (any(failed)) {
    perf[] <- NA
    perf_list[failed] <- list(perf)
  }

  perf <- do.call(c, perf_list)
  metric <- getMLObject(c(metrics)[[1]], "MLMetric")
  selected <- ifelse(metric@maximize, which.max, which.min)(perf_stats)

  list(performance = perf,
       selected = structure(selected, names = colnames(perf)[1]),
       values = perf_stats,
       metric = metric)

}
