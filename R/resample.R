#' Resample Estimation of Model Performance
#'
#' Estimation of the predictive performance of a model estimated and evaluated
#' on training and test samples generated from an observed data set.
#'
#' @name resample
#' @rdname resample-methods
#'
#' @param ... arguments passed from the generic function to its methods, from
#'   the \code{MLModel} and \code{MLModelFunction} methods to first arguments of
#'   others, and from others to the \code{ModelSpecification} method.  The
#'   first argument of each \code{fit} method is positional and, as such, must
#'   be given first in calls to them.
#' @param object model \link[=inputs]{input} or
#'   \link[=ModelSpecification]{specification}.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param input \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param model \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.  A model
#'   can be given first followed by any of the variable specifications.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#'
#' @return \code{Resample} class object.
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
resample.formula <- function(formula, data, model, ...) {
  resample(
    ModelSpecification(formula, data, model = model, control = NULL), ...
  )
}


#' @rdname resample-methods
#'
resample.matrix <- function(x, y, model, ...) {
  resample(ModelSpecification(x, y, model = model, control = NULL), ...)
}


#' @rdname resample-methods
#'
#' @details
#' Resampling stratification variables may be specified manually for
#' \code{ModelFrames} upon creation with the \code{\link[=ModelFrame]{strata}}
#' argument in their constructor.  Resampling of this class is unstratified by
#' default.
#'
resample.ModelFrame <- function(input, model, ...) {
  resample(ModelSpecification(input, model = model, control = NULL), ...)
}


#' @rdname resample-methods
#'
#' @details
#' Stratification variables may be designated in \code{recipe} specifications
#' with the \code{\link{role_case}} function.  Resampling will be unstratified
#' otherwise.
#'
resample.recipe <- function(input, model, ...) {
  resample(ModelSpecification(input, model = model, control = NULL), ...)
}


#' @rdname resample-methods
#'
resample.ModelSpecification <- function(
  object, control = MachineShop::settings("control"), ...
) {
  .resample(as.MLControl(control), object = object, ...)
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


.resample <- function(control, object, ...) {
  UseMethod(".resample")
}


.resample.BootControl <- function(control, object, progress_index = 0, ...) {
  presets <- settings()
  set.seed(control@seed)
  splits <- rsample_split(
    function(data, group = NULL, strata = NULL, ...) {
      samples <- control@samples
      suppressWarnings(
        if (length(group)) {
          group_bootstraps(data, group = group, times = samples, pool = 0)
        } else {
          bootstraps(data, times = samples, strata = strata, pool = 0)
        }
      )$splits
    }, data = object, control = control
  )
  seeds <- rand_int(length(splits))

  is_optimism_control <- is(control, "BootOptimismControl")
  if (is_optimism_control) {
    train_pred <- subsample(object, object, control)$Predicted
  }

  monitor <- new_progress_bar(
    control@monitor$progress * length(splits),
    object = object,
    index = progress_index
  )
  on.exit(monitor$pb$terminate())

  foreach(
    i = i <- seq_along(splits),
    .export = c("seq_boot", "subsample"),
    .packages = required_packages(object),
    .verbose = control@monitor$verbose,
    .options.snow = monitor$snow_opts
  ) %dopar% {
    monitor$progress()
    settings(presets)
    set.seed(seeds[i])
    train <- update(object, data = rsample::analysis(splits[[i]]))
    if (is_optimism_control) {
      subs <- subsample(train, list(object, train), control, i)
      df <- subs[[1]]
      df_boot <- subs[[2]]
      indices <- seq_boot(df_boot, df)
      df["Boot.Observed"] <- df_boot[indices, "Observed"]
      df["Boot.Predicted"] <- df_boot[indices, "Predicted"]
      df$Train.Predicted <- train_pred
      df
    } else {
      subsample(train, object, control, i)
    }
  } %>% Resample(control = control, vars = attr(splits, "vars"))
}


.resample.CVControl <- function(control, object, progress_index = 0, ...) {
  presets <- settings()
  set.seed(control@seed)
  splits <- rsample_split(
    function(data, group = NULL, strata = NULL, ...) {
      v <- control@folds
      repeats <- control@repeats
      res <- suppressWarnings(
        if (length(group)) {
          method <- "grouping"
          group_vfold_cv(
            data, group = group, v = v, repeats = repeats, pool = 0
          )
        } else {
          method <- "stratification"
          vfold_cv(data, v = v, repeats = repeats, strata = strata, pool = 0)
        }
      )$splits
      control@folds <<- as.integer(length(res) / repeats)
      if (control@folds < v) {
        throw(Warning(
          "Fewer folds constructed (", control@folds, ") than specified in ",
          class(control), " (", v, ") due to the resample ", method, " size.  ",
          "To address the issue, increase the size or use a ModelFrame or ",
          "recipe to resample without ", method, "."
        ), call = call("resample"))
      }
      res
    }, data = object, control = control
  )
  seeds <- rand_int(length(splits))

  is_optimism_control <- is(control, "CVOptimismControl")

  monitor <- new_progress_bar(
    control@monitor$progress * length(splits),
    object = object,
    index = progress_index
  )
  on.exit(monitor$pb$terminate())

  df_list <- foreach(
    i = i <- seq_along(splits),
    .export = "subsample",
    .packages = required_packages(object),
    .verbose = control@monitor$verbose,
    .options.snow = monitor$snow_opts
  ) %dopar% {
    monitor$progress()
    settings(presets)
    set.seed(seeds[i])
    train <- update(object, data = rsample::analysis(splits[[i]]))
    test <- update(object, data = rsample::assessment(splits[[i]]))
    if (is_optimism_control) {
      subs <- subsample(train, list(test, object), control, i)
      structure(subs[[1]], CV.Predicted = subs[[2]]["Predicted"])
    } else {
      subsample(train, test, control, i)
    }
  }
  res <- Resample(df_list, control = control, vars = attr(splits, "vars"))

  if (is_optimism_control) {
    pred_list <- map(attr, df_list, "CV.Predicted")
    split_factor <- rep(seq_len(control@folds), control@repeats)
    df <- split(seq_along(pred_list), split_factor) %>%
      map(function(indices) do.call(append, pred_list[indices]), .) %>%
      as.data.frame
    names(df) <- make_names_len(length(df), "CV.Predicted.")
    pred <- subsample(object, object, control)$Predicted
    df$Train.Predicted <- do.call(append, rep(list(pred), control@repeats))
    res[names(df)] <- df
  }

  res
}


.resample.OOBControl <- function(control, object, progress_index = 0, ...) {
  presets <- settings()
  set.seed(control@seed)
  splits <- rsample_split(
    function(data, group = NULL, strata = NULL, ...) {
      samples <- control@samples
      suppressWarnings(
        if (length(group)) {
          group_bootstraps(data, group = group, times = samples, pool = 0)
        } else {
          bootstraps(data, times = samples, strata = strata, pool = 0)
        }
      )$splits
    }, data = object, control = control
  )
  seeds <- rand_int(length(splits))

  monitor <- new_progress_bar(
    control@monitor$progress * length(splits),
    object = object,
    index = progress_index
  )
  on.exit(monitor$pb$terminate())

  foreach(
    i = i <- seq_along(splits),
    .export = "subsample",
    .packages = required_packages(object),
    .verbose = control@monitor$verbose,
    .options.snow = monitor$snow_opts
  ) %dopar% {
    monitor$progress()
    settings(presets)
    set.seed(seeds[i])
    train <- update(object, data = rsample::analysis(splits[[i]]))
    test <- update(object, data = rsample::assessment(splits[[i]]))
    subsample(train, test, control, i)
  } %>% Resample(control = control, vars = attr(splits, "vars"))
}


.resample.SplitControl <- function(control, object, ...) {
  set.seed(control@seed)
  split <- rsample_split(
    function(data, group = NULL, strata = NULL, ...) {
      prop <- control@prop
      if (length(group)) {
        group_initial_split(data, group = group, prop = prop, pool = 0)
      } else {
        initial_split(data, prop = prop, strata = strata, pool = 0)
      }
    }, data = object, control = control
  )
  train <- update(object, data = rsample::training(split))
  test <- update(object, data = rsample::testing(split))
  subsample(train, test, control) %>%
    Resample(control = control, vars = attr(split, "vars"))
}


.resample.TrainControl <- function(control, object, ...) {
  set.seed(control@seed)
  Resample(subsample(object, object, control), control = control)
}


Resample <- function(object, ...) {
  UseMethod("Resample")
}


Resample.data.frame <- function(
  object, ..., control, vars = NULL, .check = TRUE
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

  vars <- as_tibble(vars)
  if (is_empty(vars[["Stratification"]])) control@strata <- list()

  new("Resample", object, control = control, vars = vars, ...)
}


Resample.list <- function(object, ...) {
  Resample(do.call(append, object), ...)
}


#################### Utility Functions ####################


rsample_split <- function(fun, data, control) {
  data <- as.MLInput(data)
  df <- as.data.frame(data)
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
  res <- fun(
    df,
    group = case_comp_name(df, "groups"),
    strata = case_comp_name(df, "strata")
  )

  vars <- tibble(Case = rownames(df))
  types <- c(Grouping = "groups", Stratification = "strata")
  for (i in seq_along(types)) {
    type <- types[i]
    comp <- df[[paste0("(", type, ")")]]
    vars[[names(type)]] <- if (length(comp)) {
      as_tibble_col(comp, column_name = case_comp_name(data, type))
    }
  }
  if (length(vars)) attr(res, "vars") <- vars

  res
}


subsample <- function(train, test, control, iter = 1) {
  verbose <- control@monitor$verbose
  model_fit <- fit(train, verbose = verbose)
  times <- time(model_fit)
  if (length(times)) control@predict$times <- times

  f <- function(test) {
    weights <- if (control@weights) "weights"
    comps <- case_comps(
      model_fit, newdata = as.MLInput(test), types = c("names", weights),
      response = TRUE
    )
    df <- data.frame(
      Model = factor(as.MLModel(train)@name),
      Iteration = as.integer(iter),
      Case = comps$names
    )
    df$Observed <- comps$response
    df$Predicted <- do.call(predict, c(
      list(model_fit, as.data.frame(test), type = "raw", verbose = verbose),
      control@predict
    ))
    df$Weight <- comps$weights
    df
  }

  if (class1(test) == "list") map(f, test) else f(test)
}
