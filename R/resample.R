#' Resample Estimation of Model Performance
#'
#' Estimation of the predictive performance of a model estimated and evaluated
#' on training and test samples generated from an observed data set.
#'
#' @name resample
#' @rdname resample-methods
#'
#' @param ... arguments passed from the generic function to its methods and from
#'   the \code{MLModel} and \code{MLModelFunction} methods to others.  The
#'   first arguments of \code{resample} methods are positional and, as such,
#'   must be given first in calls to them.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param input \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param model \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.  A model
#'   can be given first followed by any of the variable specifications, and the
#'   argument can be omitted altogether in the case of
#'   \link[=ModeledInput]{modeled inputs}.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
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
resample <- function(...) {
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
  formula, data, model, control = MachineShop::settings("control"), ...
) {
  resample(as.MLInput(formula, data), model = model, control = control, ...)
}


#' @rdname resample-methods
#'
resample.matrix <- function(
  x, y, model, control = MachineShop::settings("control"), ...
) {
  resample(as.MLInput(x, y), model = model, control = control, ...)
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
  input, model = NULL, control = MachineShop::settings("control"), ...
) {
  .resample(as.MLControl(control), input = as.MLInput(input),
            model = as.MLModel(model), ...)
}


#' @rdname resample-methods
#'
#' @details
#' Stratification variables may be designated in \code{recipe} specifications
#' with the \code{\link{role_case}} function.  Resampling will be unstratified
#' otherwise.
#'
resample.recipe <- function(
  input, model = NULL, control = MachineShop::settings("control"), ...
) {
  .resample(as.MLControl(control), input = as.MLInput(input),
            model = as.MLModel(model), ...)
}


#' @rdname resample-methods
#'
resample.MLModel <- function(model, ...) {
  resample(..., model = model)
}


#' @rdname resample-methods
#'
resample.MLModelFunction <- function(model, ...) {
  resample(as.MLModel(model), ...)
}


Resamples <- function(object, ...) {
  UseMethod("Resamples")
}


Resamples.data.frame <- function(
  object, ..., control, case_comps = NULL, .check = TRUE
) {
  if (.check) {
    var_names <- c("Model", "Iteration", "Case", "Observed", "Predicted")
    missing <- missing_names(var_names, object)
    if (length(missing)) {
      throw(Error(note_items("Missing resample variable{?s}: ", missing, ".")))
    }
    object$Model <- droplevels(object$Model)
  }
  rownames(object) <- NULL

  case_comps <- as.data.frame(case_comps)
  if (is_empty(case_comps$strata)) control@strata <- list()

  new("Resamples", object, control = control, case_comps = case_comps, ...)
}


Resamples.list <- function(object, ...) {
  Resamples(do.call(append, object), ...)
}


.resample <- function(control, input, model, ...) {
  UseMethod(".resample")
}


.resample.BootControl <- function(
  control, input, model, progress_index = 0, ...
) {
  presets <- settings()
  set.seed(control@seed)
  splits <- rsample_split(
    function(..., strata = NULL) {
      bootstraps(..., times = control@samples, strata = strata)$splits
    }, data = input, control = control
  )
  seeds <- rand_int(length(splits))

  is_optimism_control <- is(control, "BootOptimismControl")
  if (is_optimism_control) {
    train_pred <- subsample(input, input, model, control)$Predicted
  }

  snow_opts <- list()
  progress <- function(n) NULL
  if (control@monitor$progress) {
    pb <- new_progress_bar(length(splits), input = input, model = model,
                           index = progress_index)
    switch(getDoParName(),
      "doSEQ"  = progress <- function(n) pb$tick(),
      "doSNOW" = snow_opts$progress <- function(n) pb$tick()
    )
    on.exit(pb$terminate())
  }

  foreach(
    i = i <- seq_along(splits),
    .export = c("seq_boot", "subsample", "subsample_input"),
    .packages = settings("require"),
    .verbose = control@monitor$verbose,
    .options.snow = snow_opts
  ) %dopar% {
    progress(i)
    settings(presets)
    set.seed(seeds[i])
    train <- subsample_input(input, rsample::analysis(splits[[i]]))
    if (is_optimism_control) {
      subs <- subsample(train, list(input, train), model, control, i)
      df <- subs[[1]]
      df_boot <- subs[[2]]
      indices <- seq_boot(df_boot, df)
      df["Boot.Observed"] <- df_boot[indices, "Observed"]
      df["Boot.Predicted"] <- df_boot[indices, "Predicted"]
      df$Train.Predicted <- train_pred
      df
    } else {
      subsample(train, input, model, control, i)
    }
  } %>% Resamples(control = control, case_comps = attr(splits, "case_comps"))
}


.resample.CVControl <- function(
  control, input, model, progress_index = 0, ...
) {
  presets <- settings()
  set.seed(control@seed)
  splits <- rsample_split(
    function(..., group = NULL, strata = NULL) {
      v <- control@folds
      repeats <- control@repeats
      if (length(group)) {
        splits <- list()
        for (i in seq_len(repeats)) {
          splits <- c(splits, group_vfold_cv(..., v = v, group = group)$splits)
        }
        splits
      } else {
        vfold_cv(..., v = v, repeats = repeats, strata = strata)$splits
      }
    }, data = input, control = control
  )
  seeds <- rand_int(length(splits))

  is_optimism_control <- is(control, "CVOptimismControl")

  snow_opts <- list()
  progress <- function(n) NULL
  if (control@monitor$progress) {
    pb <- new_progress_bar(length(splits), input = input, model = model,
                           index = progress_index)
    switch(getDoParName(),
      "doSEQ"  = progress <- function(n) pb$tick(),
      "doSNOW" = snow_opts$progress <- function(n) pb$tick()
    )
    on.exit(pb$terminate())
  }

  df_list <- foreach(
    i = i <- seq_along(splits),
    .export = c("subsample", "subsample_input"),
    .packages = settings("require"),
    .verbose = control@monitor$verbose,
    .options.snow = snow_opts
  ) %dopar% {
    progress(i)
    settings(presets)
    set.seed(seeds[i])
    train <- subsample_input(input, rsample::analysis(splits[[i]]))
    test <- subsample_input(input, rsample::assessment(splits[[i]]))
    if (is_optimism_control) {
      subs <- subsample(train, list(test, input), model, control, i)
      structure(subs[[1]], CV.Predicted = subs[[2]]["Predicted"])
    } else {
      subsample(train, test, model, control, i)
    }
  }
  res <- Resamples(df_list, control = control,
                   case_comps = attr(splits, "case_comps"))

  if (is_optimism_control) {
    pred_list <- map(attr, df_list, "CV.Predicted")
    split_factor <- rep(seq_len(control@folds), control@repeats)
    df <- split(seq_along(pred_list), split_factor) %>%
      map(function(indices) do.call(append, pred_list[indices]), .) %>%
      as.data.frame
    names(df) <- make_names_len(length(df), "CV.Predicted.")
    pred <- subsample(input, input, model, control)$Predicted
    df$Train.Predicted <- do.call(append, rep(list(pred), control@repeats))
    res[names(df)] <- df
  }

  res
}


.resample.OOBControl <- function(
  control, input, model, progress_index = 0, ...
) {
  presets <- settings()
  set.seed(control@seed)
  splits <- rsample_split(
    function(..., strata = NULL) {
      bootstraps(..., times = control@samples, strata = strata)$splits
    }, data = input, control = control
  )
  seeds <- rand_int(length(splits))

  snow_opts <- list()
  progress <- function(n) NULL
  if (control@monitor$progress) {
    pb <- new_progress_bar(length(splits), input = input, model = model,
                           index = progress_index)
    switch(getDoParName(),
      "doSEQ"  = progress <- function(n) pb$tick(),
      "doSNOW" = snow_opts$progress <- function(n) pb$tick()
    )
    on.exit(pb$terminate())
  }

  foreach(
    i = i <- seq_along(splits),
    .export = c("subsample", "subsample_input"),
    .packages = settings("require"),
    .verbose = control@monitor$verbose,
    .options.snow = snow_opts
  ) %dopar% {
    progress(i)
    settings(presets)
    set.seed(seeds[i])
    train <- subsample_input(input, rsample::analysis(splits[[i]]))
    test <- subsample_input(input, rsample::assessment(splits[[i]]))
    subsample(train, test, model, control, i)
  } %>% Resamples(control = control, case_comps = attr(splits, "case_comps"))
}


.resample.SplitControl <- function(control, input, model, ...) {
  set.seed(control@seed)
  split <- rsample_split(
    function(...) initial_split(..., prop = control@prop),
    data = input, control = control
  )
  train <- subsample_input(input, rsample::training(split))
  test <- subsample_input(input, rsample::testing(split))
  subsample(train, test, model, control) %>%
    Resamples(control = control, case_comps = attr(split, "case_comps"))
}


.resample.TrainControl <- function(control, input, model, ...) {
  set.seed(control@seed)
  Resamples(subsample(input, input, model, control), control = control)
}


#################### Utility Functions ####################


rsample_data <- function(input, ...) UseMethod("rsample_data")
rsample_data.ModelFrame <- function(input, ...) asS3(input)
rsample_data.ModelRecipe <- function(input, ...) as.data.frame(input)


rsample_split <- function(fun, data, control) {
  df <- rsample_data(data)
  formal_names <- names(formals(fun))
  df[["(groups)"]] <- if ("group" %in% formal_names) case_groups(data)
  df[["(strata)"]] <- if ("strata" %in% formal_names) {
    do.call(case_strata, c(list(data), control@strata))
  }
  if (length(df[["(groups)"]]) && length(df[["(strata)"]])) {
    throw(LocalWarning(
      "Case groups and strata are both specified for resampling; ",
      "only the strata will be used."
    ))
    df[["(groups)"]] <- NULL
  }
  res <- suppressWarnings(fun(df, group = case_comp_name(df, "groups"),
                              strata = case_comp_name(df, "strata"), pool = 0))

  comps <- data.frame(row.names = rownames(df))
  for (type in c("groups", "strata")) {
    comp <- df[[paste0("(", type, ")")]]
    comps[[type]] <- if (length(comp)) {
      structure(data.frame(comp), names = case_comp_name(data, type))
    }
  }
  if (length(comps)) attr(res, "case_comps") <- comps

  res
}


subsample <- function(train, test, model, control, iter = 1) {
  model_fit <- fit(train, model)
  times <- time(model_fit)
  if (length(times)) control@predict$times <- times

  f <- function(test) {
    weights <- if (control@weights) "weights"
    comps <- case_comps(model_fit, test, c("names", weights), response = TRUE)
    df <- data.frame(Model = factor(model@name),
                     Iteration = as.integer(iter),
                     Case = comps$names,
                     stringsAsFactors = FALSE)
    df$Observed <- comps$response
    predict_args <- list(model_fit, as.data.frame(test), type = "prob")
    df$Predicted <- do.call(predict, c(predict_args, control@predict))
    df$Weight <- comps$weights
    df
  }

  if (class1(test) == "list") map(f, test) else f(test)
}


subsample_input <- function(x, ...) UseMethod("subsample_input")
subsample_input.ModelFrame <- function(x, data, ...) as(data, class(x))
subsample_input.ModelRecipe <- function(x, data, ...) recipe(x, data)


resample_selection <- function(
  object, ..., grid = tibble(), params = TrainingParams(), id = character(),
  name = character()
) {

  grid <- as(grid, "tbl_df")
  metrics <- params@metrics
  stat <- check_stat(params@stat, convert = TRUE)
  throw(check_assignment(stat))

  perf_list <- list()
  perf_stats <- list()
  err_msgs <- character()
  ind <- new_progress_index(name = name, max = nrow(grid))
  ind_names <- make_unique(rep(training_names(object), length = max(ind)))
  while (ind < max(ind)) {
    ind <- ind + 1
    ind_name <- ind_names[ind]

    res <- try(
      resample(update(object, grid[as.integer(ind), ]), ...,
               control = params@control, progress_index = ind),
      silent = TRUE
    )

    if (is(res, "try-error")) {
      perf_list[[ind_name]] <- NA
      perf_stats[[ind_name]] <- NA
      err_msgs[ind_name] <- conditionMessage(attr(res, "condition"))
      next
    }

    if (is.null(metrics)) {
      metrics <- get_perf_metrics(res$Observed, res$Predicted)
    }

    perf <- performance(res, metrics = metrics, cutoff = params@cutoff)
    perf_list[[ind_name]] <- perf
    perf_stats[[ind_name]] <- apply(perf, 2, function(x) stat(na.omit(x)))
  }

  failed <- is.na(perf_list)
  err_msgs <- paste0(names(err_msgs), ": ", err_msgs, collapse = "\n")
  if (all(failed)) {
    throw(LocalError("Resampling failed for all models.\n", err_msgs))
  } else if (any(failed)) {
    throw(LocalWarning("Resampling failed for some models.\n", err_msgs))
    perf[] <- NA
    perf_list[failed] <- list(perf)
    perf_stats[failed] <- list(perf[1, ])
  }

  perf <- do.call(c, perf_list)
  perf_stats <- do.call(rbind, perf_stats)
  metric <- as.MLMetric(c(metrics)[[1]])
  selected <- (if (metric@maximize) which.max else which.min)(perf_stats[, 1])

  res <- TrainingStep(
    id = id,
    name = name,
    grid = tibble(
      name = names(perf_list),
      selected = FALSE,
      params = grid,
      metrics = as_tibble(perf_stats)
    ),
    performance = perf
  )
  res@grid$selected[selected] <- TRUE
  res

}


training_names <- function(x, ...) UseMethod("training_names")
training_names.MLInput <- function(x, ...) class(x)
training_names.MLModel <- function(x, ...) x@name
training_names.SelectedInput <- function(x, ...) names(x@inputs)
training_names.SelectedModel <- function(x, ...) names(x@models)
