#' MachineShop Settings
#'
#' Allow the user to view or change global settings which affect default
#' behaviors of functions in the \pkg{MachineShop} package.
#'
#' @param ... character names of settings to view, \code{name = value} pairs
#' giving the values of settings to change, a vector of these, \code{"reset"}
#' to restore all package defaults, or no arguments to view all settings.
#' Partial matching of setting names is supported.
#'
#' @return The setting value if only one is specified to view.  Otherwise, a
#' list of the values of specified settings as they existed prior to any
#' requested changes.  Such a list can be passed as an argument to
#' \code{settings} to restore their values.
#'
#' @section Settings:
#'
#' \describe{
#'   \item{\code{\link[=controls]{control}}}{function, function name, or call
#'     defining a default resampling method [default: \code{"CVControl"}].}
#'   \item{\code{cutoff}}{numeric (0, 1) threshold above which binary factor
#'     probabilities are classified as events and below which survival
#'     probabilities are classified [default: 0.5].}
#'   \item{\code{dist.Surv}}{character string specifying distributional
#'     approximations to estimated survival curves for predicting survival
#'     means.  Choices are \code{"empirical"} for the Kaplan-Meier estimator,
#'     \code{"exponential"}, or \code{"weibull"} (default).}
#'   \item{\code{dist.SurvProbs}}{character string specifying distributional
#'     approximations to estimated survival curves for predicting survival
#'     events/probabilities.  Choices are \code{"empirical"} (default) for the
#'     Kaplan-Meier estimator, \code{"exponential"}, or \code{"weibull"}.}
#'   \item{\code{grid}}{number of parameter-specific values to generate
#'     automatically for \link[=TunedModel]{tuning} of models that have
#'     pre-defined grids or a \code{\link{Grid}} function, function name, or
#'     call [default: 3].}
#'   \item{\code{max.print}}{number of models or data rows to show with print
#'     methods or \code{Inf} to show all [default: 10].}
#'   \item{\code{method.EmpiricalSurv}}{character string specifying the
#'     empirical method of estimating baseline survival curves for Cox
#'     proportional hazards-based models.  Choices are \code{"breslow"},
#'     \code{"efron"} (default), or \code{"fleming-harrington"}.}
#'   \item{\code{metrics.ConfusionMatrix}}{function, function name, or vector of
#'     these with which to calculate \link{performance} \link{metrics} for
#'     confusion matrices [default: \code{c(Accuracy = "accuracy", Kappa =
#'     "kappa2", `Weighted Kappa` = "weighted_kappa2", Sensitivity =
#'     "sensitivity", Specificity = "specificity")}].}
#'   \item{\code{metrics.factor}}{function, function name, or vector of these
#'     with which to calculate \link{performance} \link{metrics} for factor
#'     responses [default: \code{c(Brier = "brier", Accuracy = "accuracy",
#'     Kappa = "kappa2", `Weighted Kappa` = "weighted_kappa2", `ROC AUC` =
#'     "roc_auc", Sensitivity = "sensitivity", Specificity = "specificity")}].}
#'   \item{\code{metrics.matrix}}{function, function name, or vector of these
#'     with which to calculate \link{performance} \link{metrics} for matrix
#'     responses [default: \code{c(RMSE = "rmse", R2 = "r2", MAE = "mae")}].}
#'   \item{\code{metrics.numeric}}{function, function name, or vector of these
#'     with which to calculate \link{performance} \link{metrics} for numeric
#'     responses [default: \code{c(RMSE = "rmse", R2 = "r2", MAE = "mae")}].}
#'   \item{\code{metrics.Surv}}{function, function name, or vector of these with
#'     which to calculate \link{performance} \link{metrics} for survival
#'     responses [default: \code{c(`C-Index` = "cindex", Brier = "brier",
#'     `ROC AUC` = "roc_auc", Accuracy = "accuracy")}].}
#'   \item{\code{require}}{names of installed packages to load during parallel
#'     execution of resampling algorithms [default: \code{c("MachineShop",
#'     "survival", "recipes")}].}
#'   \item{\code{reset}}{character names of settings to reset to their default
#'     values.}
#'   \item{\code{stat.Curves}}{function or character string naming a function
#'     to compute one \link{summary} statistic at each cutoff value of resampled
#'     metrics in performance curves, or \code{NULL} for resample-specific
#'     metrics [default: \code{"base::mean"}].}
#'   \item{\code{stat.Resamples}}{function or character string naming a function
#'     to compute one summary statistic to control the ordering of models in
#'     \link[=plot]{plots} [default: \code{"base::mean"}].}
#'   \item{\code{stat.train}}{function or character string naming a function
#'     to compute one summary statistic on resampled performance metrics for
#'     \link[=SelectedModel]{model selection}, \link[=TunedModel]{model tuning},
#'     and \link[=TunedRecipe]{recipe tuning} [default: \code{"base::mean"}].}
#'   \item{\code{stats.PartialDependence}}{function, function name, or vector of
#'     these with which to compute \link[=dependence]{partial dependence}
#'     summary statistics [default: \code{c(Mean = "base::mean")}].}
#'   \item{\code{stats.Resamples}}{function, function name, or vector of these
#'     with which to compute \link{summary} statistics on resampled performance
#'     metrics [default: \code{c(Mean = "base::mean", Median = "stats::median",
#'     SD = "stats::sd", Min = "base::min", Max = "base::max")}].
#'   }
#' }
#'
#' @examples
#' ## View all current settings
#' settings()
#'
#' ## Change settings
#' presets <- settings(control = "BootControl", grid = 10)
#'
#' ## View one setting
#' settings("control")
#'
#' ## View multiple settings
#' settings("control", "grid")
#'
#' ## Restore the previous settings
#' settings(presets)
#'
settings <- function(...) {

  args <- list(...)
  if (length(args) == 1 && is.null(names(args)) && is.vector(args[[1]])) {
    args <- as.list(args[[1]])
  }

  global_settings <- MachineShop_global$settings
  global_values <- lapply(global_settings, getElement, name = "value")
  global_checks <- lapply(global_settings, getElement, name = "check")

  if (!length(args)) {
    return(global_values)
  } else if (identical(args, list("reset"))) {
    settings(reset = names(.global_defaults))
    return(invisible(global_values))
  }

  args_names <- names(args)
  if (is.null(args_names)) args_names <- character(length(args))
  args_names_nzchar <- nzchar(args_names)

  is_get_args <- !args_names_nzchar & sapply(args, is.character)
  args_names[is_get_args] <- unlist(args[is_get_args])

  settings_pmatch <- pmatch(args_names, names(global_settings))
  valid_settings <- !is.na(settings_pmatch)
  for (name in args_names[!valid_settings]) {
    warning("'", name, "' is not a MachineShop setting")
  }

  presets <- global_values[settings_pmatch[valid_settings]]

  which_set_args <- which(args_names_nzchar & valid_settings)
  for (index in which_set_args) {
    global_name <- names(global_settings)[settings_pmatch[index]]
    value <- global_checks[[global_name]](args[[index]])
    if (is(value, "DomainError")) {
      stop("MachineShop '", global_name, "' setting ", value$message)
    }
    MachineShop_global$settings[[global_name]]$value <- value
  }

  if (any(args_names_nzchar)) {
    invisible(presets)
  } else if (length(args) > 1) {
    presets
  } else if (length(presets)) {
    presets[[1]]
  } else {
    NULL
  }

}


#################### Settings Utility Functions ####################


check_match <- function(choices) {
  function(x) {
    result <- try(match.arg(x, choices), silent = TRUE)
    if (is(result, "try-error")) {
      DomainError(x, "must be one of ", toString(paste0("\"", choices, "\"")))
    } else result
  }
}


check_metrics <- function(x) {
  result <- try(lapply(c(x), getMLObject, class = "MLMetric"), silent = TRUE)
  if (is(result, "try-error")) {
    DomainError(x, "must be a metrics function, function name, ",
                   "or vector of these")
  } else x
}


check_stat <- function(x) {
  result <- try(fget(x)(1:5), silent = TRUE)
  if (is(result, "try-error") || !is.numeric(result) || length(result) != 1) {
    DomainError(x, "must be a statistic function or function name")
  } else x
}


check_stats <- function(x) {
  result <- try(list2function(x)(1:5), silent = TRUE)
  if (is(result, "try-error") || !is.numeric(result)) {
    DomainError(x, "must be a statistics function, function name, ",
                   "or vector of these")
  } else x
}


#################### Global Environment ####################


MachineShop_global <- as.environment(list(

  settings = list(

    control = list(
      value = "CVControl",
      check = function(x) {
        result <- try(getMLObject(x, "MLControl"), silent = TRUE)
        if (is(result, "try-error")) {
          DomainError(x, "must be an MLControl object, function, ",
                         "or function name")
        } else x
      }
    ),

    cutoff = list(
      value = 0.5,
      check = function(x) {
        if (!is.numeric(x) || length(x) != 1 || x <= 0 || x >= 1) {
          DomainError(x, "must be a numeric value in (0, 1)")
        } else x
      }
    ),

    dist.Surv = list(
      value = "weibull",
      check = check_match(c("weibull", "exponential", "empirical"))
    ),

    dist.SurvProbs = list(
      value = "empirical",
      check = check_match(c("empirical", "weibull", "exponential"))
    ),

    grid = list(
      value = 3,
      check = function(x) {
        if (is(x, "Grid")) {
          x
        } else if (identical(x, "Grid") || identical(x, Grid)) {
          Grid()
        } else {
          result <- try(Grid(x), silent = TRUE)
          if (is(result, "try-error")) {
            DomainError(x, "must be a positive numeric value or ",
                           "a Grid function, function name, or call")
          } else result
        }
      }
    ),

    max.print = list(
      value = 10,
      check = function(x) {
        result <- try(floor(x[[1]]), silent = TRUE)
        if (is(result, "try-error") || result <= 0) {
          DomainError(x, "must be a positive number")
        } else result
      }
    ),

    method.EmpiricalSurv = list(
      value = "efron",
      check = check_match(c("efron", "breslow", "fleming-harrington"))
    ),

    metrics.ConfusionMatrix = list(
      value = c("Accuracy" = "accuracy",
                "Kappa" = "kappa2",
                "Weighted Kappa" = "weighted_kappa2",
                "Sensitivity" = "sensitivity",
                "Specificity" = "specificity"),
      check = check_metrics
    ),

    metrics.factor = list(
      value = c("Brier" = "brier",
                "Accuracy" = "accuracy",
                "Kappa" = "kappa2",
                "Weighted Kappa" = "weighted_kappa2",
                "ROC AUC" = "roc_auc",
                "Sensitivity" = "sensitivity",
                "Specificity" = "specificity"),
      check = check_metrics
    ),

    metrics.matrix = list(
      value = c("RMSE" = "rmse",
                "R2" = "r2",
                "MAE" = "mae"),
      check = check_metrics
    ),

    metrics.numeric = list(
      value = c("RMSE" = "rmse",
                "R2" = "r2",
                "MAE" = "mae"),
      check = check_metrics
    ),

    metrics.Surv = list(
      value = c("C-Index" = "cindex",
                "Brier" = "brier",
                "ROC AUC" = "roc_auc",
                "Accuracy" = "accuracy"),
      check = check_metrics
    ),

    require = list(
      value = c("MachineShop", "survival", "recipes"),
      check = function(x) {
        x <- setdiff(x, .global_defaults$require)
        available <- vapply(x, requireNamespace, logical(1), quietly = TRUE)
        if (!all(available)) {
          missing <- x[!available]
          msg <- paste0(plural_suffix("given unavailable package", missing),
                        ": ", toString(missing))
          DomainError(x, msg)
        } else c(x, .global_defaults$require)
      }
    ),

    reset = list(
      value = character(),
      check = function(x) {
        if (is.character(x)) {
          setting_names <- names(.global_defaults)
          reset_names <- setting_names[charmatch(x, setting_names, nomatch = 0)]
          for (name in reset_names) {
            value <- .global_defaults[[name]]
            MachineShop_global$settings[[name]]$value <- value
          }
          if ("reset" %in% reset_names) .global_defaults$reset else reset_names
        } else {
          DomainError(x, "must be a character value or vector")
        }
      }
    ),

    stat.Curves = list(
      value = "base::mean",
      check = function(x) {
        if (!is.null(x) && is(check_stat(x), "error")) {
          DomainError(x, "must be a statistics function, function name, ",
                         "vector of these, or NULL")
        } else x
      }
    ),

    stat.Resamples = list(
      value = "base::mean",
      check = check_stat
    ),

    stat.train = list(
      value = "base::mean",
      check = check_stat
    ),

    stats.PartialDependence = list(
      value = c("Mean" = "base::mean"),
      check = check_stats

    ),

    stats.Resamples = list(
      value = c("Mean" = "base::mean",
                "Median" = "stats::median",
                "SD" = "stats::sd",
                "Min" = "base::min",
                "Max" = "base::max"),
      check = check_stats
    )

  )

))


.global_defaults <- settings()
