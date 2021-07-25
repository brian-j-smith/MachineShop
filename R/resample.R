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
#'   function or object may be given first followed by the input specification
#'   and \code{control} value.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} containing observed predictors and
#'   outcomes.
#' @param model \link[=models]{model} function, function name, or object;
#'   ignored and can be omitted when resampling
#'   \link[=ModeledInput]{modeled inputs}.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param ... arguments passed to other methods.
#'
#' @return \code{Resamples} class object.
#'
#' @seealso \code{\link{c}}, \code{\link{metrics}}, \code{\link{performance}},
#' \code{\link{plot}}, \code{\link{summary}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
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
#' }
#'
resample <- function(x, ...) {
  UseMethod("resample")
}


#' @rdname resample-methods
#'
#' @details
#' Stratified resampling is performed automatically for the \code{formula} and
#' \code{matrix} methods according to the type of response variable.  In
#' general, strata are constructed from numeric proportions for
#' \code{\link{BinomialVariate}}; original values for \code{character},
#' \code{factor}, \code{logical}, and \code{ordered}; first columns of values
#' for \code{matrix}; original values for \code{numeric}; and numeric times
#' within event statuses for \code{Surv}.  Numeric values are stratified into
#' quantile bins and categorical values into factor levels defined by
#' \code{\link{MLControl}}.
#'
resample.formula <- function(
  x, data, model, control = MachineShop::settings("control"), ...
) {
  mf <- ModelFrame(x, data, na.rm = FALSE, strata = response(x, data))
  resample(mf, model, control, ...)
}


#' @rdname resample-methods
#'
resample.matrix <- function(
  x, y, model, control = MachineShop::settings("control"), ...
) {
  mf <- ModelFrame(x, y, na.rm = FALSE, strata = y)
  resample(mf, model, control, ...)
}


#' @rdname resample-methods
#'
#' @details
#' Resampling stratification variables may be specified manually for
#' \code{ModelFrames} upon creation with the \code{\link[=ModelFrame]{strata}}
#' argument in their constructor.  Resampling of this class is unstratified by
#' default.
#'
resample.ModelFrame <- function(
  x, model, control = MachineShop::settings("control"), ...
) {
  if (missing(model)) model <- NullModel()
  .resample(get_MLControl(control), x, model, ...)
}


#' @rdname resample-methods
#'
#' @details
#' Stratification variables may be designated in \code{recipe} specifications
#' with the \code{\link{role_case}} function.  Resampling will be unstratified
#' otherwise.
#'
resample.recipe <- function(
  x, model, control = MachineShop::settings("control"), ...
) {
  if (missing(model)) model <- NullModel()
  .resample(get_MLControl(control), ModelRecipe(x), model, ...)
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
    var_names <- c("Model", "Resample", "Case", "Observed", "Predicted")
    missing <- missing_names(var_names, object)
    if (length(missing)) {
      throw(Error(label_items("missing resample variable", missing)))
    }
    object$Model <- droplevels(object$Model)
  }
  rownames(object) <- NULL
  new("Resamples", object, strata = as.data.frame(strata), ...)
}


Resamples.list <- function(object, ...) {
  Resamples(do.call(append, object), ...)
}


.resample <- function(object, x, model, ...) {
  UseMethod(".resample")
}


.resample.MLBootControl <- function(object, x, model, progress_index = 0, ...) {
  presets <- settings()
  set.seed(object@seed)
  splits <- rsample_split(function(...) bootstraps(...)$splits,
                          data = x,
                          times = object@samples,
                          control = object)
  seeds <- sample.int(.Machine$integer.max, length(splits))

  is_optimism_control <- is(object, "MLBootOptimismControl")
  if (is_optimism_control) {
    train_pred <- subsample(x, x, model, object)$Predicted
  }

  snow_opts <- list()
  progress <- function(n) NULL
  if (object@monitor$progress) {
    pb <- new_progress_bar(length(splits), input = x, model = model,
                           index = progress_index)
    on.exit(pb$terminate())
    switch(getDoParName(),
      "doSEQ"  = progress <- function(n) pb$tick(),
      "doSNOW" = snow_opts$progress <- function(n) pb$tick()
    )
  }

  foreach(
    i = seq_along(splits),
    .export = c("seq_boot", "subsample", "subsample_data"),
    .packages = settings("require"),
    .verbose = object@monitor$verbose,
    .options.snow = snow_opts
  ) %dopar% {
    progress(i)
    settings(presets)
    set.seed(seeds[i])
    train <- subsample_data(rsample::analysis, splits[[i]], x)
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
  } %>% Resamples(control = object, strata = attr(splits, "strata"))
}


.resample.MLCVControl <- function(object, x, model, progress_index = 0, ...) {
  presets <- settings()
  set.seed(object@seed)
  splits <- rsample_split(function(...) vfold_cv(...)$splits,
                          data = x,
                          v = object@folds,
                          repeats = object@repeats,
                          control = object)
  seeds <- sample.int(.Machine$integer.max, length(splits))

  is_optimism_control <- is(object, "MLCVOptimismControl")

  snow_opts <- list()
  progress <- function(n) NULL
  if (object@monitor$progress) {
    pb <- new_progress_bar(length(splits), input = x, model = model,
                           index = progress_index)
    on.exit(pb$terminate())
    switch(getDoParName(),
      "doSEQ"  = progress <- function(n) pb$tick(),
      "doSNOW" = snow_opts$progress <- function(n) pb$tick()
    )
  }

  df_list <- foreach(
    i = seq_along(splits),
    .export = c("subsample", "subsample_data"),
    .packages = settings("require"),
    .verbose = object@monitor$verbose,
    .options.snow = snow_opts
  ) %dopar% {
    progress(i)
    settings(presets)
    set.seed(seeds[i])
    train <- subsample_data(rsample::analysis, splits[[i]], x)
    test <- subsample_data(rsample::assessment, splits[[i]], x)
    if (is_optimism_control) {
      subs <- subsample(train, list(test, x), model, object, i)
      structure(subs[[1]], CV.Predicted = subs[[2]]["Predicted"])
    } else {
      subsample(train, test, model, object, i)
    }
  }
  res <- Resamples(df_list, control = object, strata = attr(splits, "strata"))

  if (is_optimism_control) {
    pred_list <- map(attr, df_list, "CV.Predicted")
    split_factor <- rep(seq_len(object@folds), object@repeats)
    df <- split(seq_along(pred_list), split_factor) %>%
      map(function(indices) do.call(append, pred_list[indices]), .) %>%
      as.data.frame
    names(df) <- paste0("CV.Predicted.", seq_along(df))
    pred <- subsample(x, x, model, object)$Predicted
    df$Train.Predicted <- do.call(append, rep(list(pred), object@repeats))
    res[names(df)] <- df
  }

  res
}


.resample.MLOOBControl <- function(object, x, model, progress_index = 0, ...) {
  presets <- settings()
  set.seed(object@seed)
  splits <- rsample_split(function(...) bootstraps(...)$splits,
                          data = x,
                          times = object@samples,
                          control = object)
  seeds <- sample.int(.Machine$integer.max, length(splits))

  snow_opts <- list()
  progress <- function(n) NULL
  if (object@monitor$progress) {
    pb <- new_progress_bar(length(splits), input = x, model = model,
                           index = progress_index)
    on.exit(pb$terminate())
    switch(getDoParName(),
      "doSEQ"  = progress <- function(n) pb$tick(),
      "doSNOW" = snow_opts$progress <- function(n) pb$tick()
    )
  }

  foreach(
    i = seq_along(splits),
    .export = c("subsample", "subsample_data"),
    .packages = settings("require"),
    .verbose = object@monitor$verbose,
    .options.snow = snow_opts
  ) %dopar% {
    progress(i)
    settings(presets)
    set.seed(seeds[i])
    train <- subsample_data(rsample::analysis, splits[[i]], x)
    test <- subsample_data(rsample::assessment, splits[[i]], x)
    subsample(train, test, model, object, i)
  } %>% Resamples(control = object, strata = attr(splits, "strata"))
}


.resample.MLSplitControl <- function(object, x, model, ...) {
  set.seed(object@seed)
  split <- rsample_split(initial_split,
                         data = x,
                         prop = object@prop,
                         control = object)
  train <- subsample_data(rsample::training, split, x)
  test <- subsample_data(rsample::testing, split, x)
  subsample(train, test, model, object) %>%
    Resamples(control = object, strata = attr(split, "strata"))
}


.resample.MLTrainControl <- function(object, x, model, ...) {
  set.seed(object@seed)
  Resamples(subsample(x, x, model, object), control = object)
}


#################### Utility Functions ####################


rsample_data <- function(x, ...) UseMethod("rsample_data")
rsample_data.ModelFrame <- function(x, ...) asS3(x)
rsample_data.ModelRecipe <- function(x, ...) as.data.frame(x)


rsample_split <- function(fun, data, control, ...) {
  df <- rsample_data(data)
  strata <- do.call(case_strata, c(list(data), control@strata))
  df[["(strata)"]] <- strata
  res <- suppressWarnings(fun(df, ..., strata = case_strata_name(df), pool = 0))
  if (length(strata)) {
    attr(res, "strata") <- data.frame(strata, row.names = rownames(df))
    names(attr(res, "strata")) <- case_strata_name(data)
  }
  res
}


subsample <- function(train, test, model, control, id = 1) {
  model <- get_MLModel(model)

  model_fit <- fit(train, model)
  times <- time(model_fit)
  if (length(times)) control@predict$times <- times

  f <- function(test) {
    comps <- case_comps(model_fit, test, response = TRUE)
    df <- data.frame(Model = factor(model@name),
                     Resample = as.integer(id),
                     Case = comps$names,
                     stringsAsFactors = FALSE)
    df$Observed <- comps$response
    predict_args <- list(model_fit, as.data.frame(test), type = "prob")
    df$Predicted <- do.call(predict, c(predict_args, control@predict))
    df$Weight <- comps$weights
    df
  }

  if (class(test)[1] == "list") map(f, test) else f(test)
}


subsample_data <- function(fun, x, object) {
  classes <- c("ModelFrame", "ModelRecipe")
  switch(which(c(map_logi(is, list(object), classes), TRUE))[1],
    as(fun(x), class(object)[1]),
    recipe(object, fun(x)),
    throw(TypeError(object, classes))
  )
}


resample_selection <- function(x, update, params, ..., class) {

  metrics <- params$metrics
  stat <- fget(params$stat)

  perf_list <- list()
  perf_stats <- numeric()
  err_msgs <- character()
  i <- new_progress_index(names = class, max = length(x))
  while (i < max(i)) {
    i <- i + 1
    name <- names(x)[i]

    res <- try(
      resample(update(x[[name]]), ..., control = params$control,
               progress_index = i),
      silent = TRUE
    )

    if (is(res, "try-error")) {
      perf_list[[name]] <- NA
      perf_stats[name] <- NA
      err_msgs[name] <- conditionMessage(attr(res, "condition"))
      next
    }

    if (is.null(metrics)) {
      method <- get_S3method(performance, res$Observed)
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
  err_msgs <- paste0(names(err_msgs), ": ", err_msgs, collapse = "\n")
  if (all(failed)) {
    throw(LocalError("Resampling failed for all models.\n", err_msgs))
  } else if (any(failed)) {
    throw(LocalWarning("Resampling failed for some models.\n", err_msgs))
    perf[] <- NA
    perf_list[failed] <- list(perf)
  }

  perf <- do.call(c, perf_list)
  metric <- get_MLMetric(c(metrics)[[1]])
  selected <- (if (metric@maximize) which.max else which.min)(perf_stats)

  list(performance = perf,
       selected = structure(selected, names = colnames(perf)[1]),
       values = perf_stats,
       metric = metric)

}
