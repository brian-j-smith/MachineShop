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
#'   \item{\code{\link[=controls]{control}}}{function, function name, or object
#'     defining a default resampling method [default: \code{"CVControl"}].}
#'   \item{\code{cutoff}}{numeric (0, 1) threshold above which binary factor
#'     probabilities are classified as events and below which survival
#'     probabilities are classified [default: 0.5].}
#'   \item{\code{distr.SurvMeans}}{character string specifying distributional
#'     approximations to estimated survival curves for predicting survival
#'     means.  Choices are \code{"empirical"} for the Kaplan-Meier estimator,
#'     \code{"exponential"}, \code{"rayleigh"}, or \code{"weibull"} (default).}
#'   \item{\code{distr.SurvProbs}}{character string specifying distributional
#'     approximations to estimated survival curves for predicting survival
#'     events/probabilities.  Choices are \code{"empirical"} (default) for the
#'     Kaplan-Meier estimator, \code{"exponential"}, \code{"rayleigh"}, or
#'     \code{"weibull"}.}
#'   \item{\code{grid}}{\code{size} argument to \code{\link{TuningGrid}}
#'     indicating the number of parameter-specific values to generate
#'     automatically for \link[=TunedModel]{tuning} of models that have
#'     pre-defined grids or a \code{\link{TuningGrid}} function, function name,
#'     or object [default: 3].}
#'   \item{\code{method.EmpiricalSurv}}{character string specifying the
#'     empirical method of estimating baseline survival curves for Cox
#'     proportional hazards-based models.  Choices are \code{"breslow"} or
#'     \code{"efron"} (default).}
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
#'   \item{\code{print_max}}{number of models or data rows to show with print
#'     methods or \code{Inf} to show all [default: 10].}
#'   \item{\code{require}}{names of installed packages to load during parallel
#'     execution of resampling algorithms [default: \code{"MachineShop"}].}
#'   \item{\code{reset}}{character names of settings to reset to their default
#'     values.}
#'   \item{\code{RHS.formula}}{non-modifiable character vector of operators and
#'     functions allowed in traditional formula specifications.}
#'   \item{\code{stat.Curve}}{function or character string naming a function
#'     to compute one \link{summary} statistic at each cutoff value of resampled
#'     metrics in performance curves, or \code{NULL} for resample-specific
#'     metrics [default: \code{"base::mean"}].}
#'   \item{\code{stat.Resample}}{function or character string naming a function
#'     to compute one summary statistic to control the ordering of models in
#'     \link[=plot]{plots} [default: \code{"base::mean"}].}
#'   \item{\code{stat.TrainingParams}}{function or character string naming a function
#'     to compute one summary statistic on resampled performance metrics for
#'     input \link[=SelectedInput]{selection} or \link[=TunedInput]{tuning} or
#'     for model \link[=SelectedModel]{selection} or \link[=TunedModel]{tuning}
#'     [default: \code{"base::mean"}].}
#'   \item{\code{stats.PartialDependence}}{function, function name, or vector of
#'     these with which to compute \link[=dependence]{partial dependence}
#'     summary statistics [default: \code{c(Mean = "base::mean")}].}
#'   \item{\code{stats.Resample}}{function, function name, or vector of these
#'     with which to compute \link{summary} statistics on resampled performance
#'     metrics [default: \code{c(Mean = "base::mean", Median = "stats::median",
#'     SD = "stats::sd", Min = "base::min", Max = "base::max")}].}
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

  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- character(length(args))
  arg_names_nzchar <- nzchar(arg_names)

  is_get_args <- !arg_names_nzchar & vapply(args, is.character, logical(1))
  arg_names[is_get_args] <- unlist(args[is_get_args])

  settings_pmatch <- pmatch(arg_names, names(global_settings))
  valid_settings <- !is.na(settings_pmatch)
  for (name in arg_names[!valid_settings]) {
    throw(Warning("Argument `", name, "` is not a MachineShop setting."))
  }

  presets <- global_values[settings_pmatch[valid_settings]]

  which_set_args <- which(arg_names_nzchar & valid_settings)
  for (index in which_set_args) {
    global_name <- as.name(names(global_settings)[settings_pmatch[index]])
    value <- global_checks[[global_name]](args[[index]])
    eval(substitute(
      throw(check_assignment(global_name, value), call = sys.call(-2))
    ))
    MachineShop_global$settings[[global_name]]$value <- value
  }

  if (any(arg_names_nzchar)) {
    invisible(presets)
  } else if (length(args) > 1) {
    presets
  } else if (length(presets)) {
    presets[[1]]
  } else {
    NULL
  }

}


#################### Global Environment ####################


MachineShop_global <- as.environment(list(

  settings = list(

    control = list(
      value = "CVControl",
      check = function(x) {
        tryCatch(
          {
            as.MLControl(x)
            x
          },
          error = function(cond) {
            DomainError(
              x, "must be an MLControl object, function, or function name"
            )
          }
        )
      }
    ),

    cutoff = list(
      value = 0.5,
      check = function(x) {
        check_numeric(x, bounds = c(0, 1), include = FALSE, size = 1)
      }
    ),

    distr.SurvMeans = list(
      value = "weibull",
      check = function(x) {
        check_match(x, c("weibull", "exponential", "rayleigh", "empirical"))
      }
    ),

    distr.SurvProbs = list(
      value = "empirical",
      check = function(x) {
        check_match(x, c("empirical", "weibull", "exponential", "rayleigh"))
      }
    ),

    grid = list(
      value = 3,
      check = check_grid
    ),

    method.EmpiricalSurv = list(
      value = "efron",
      check = function(x) {
        check_match(x, c("efron", "breslow"))
      }
    ),

    metrics = list(
      value = c(
        "accuracy",
        "auc",
        "brier",
        "cindex",
        "cross_entropy",
        "f_score",
        "fnr",
        "fpr",
        "gini",
        "kappa2",
        "mae",
        "mse",
        "msle",
        "npv",
        "ppr",
        "ppv",
        "pr_auc",
        "precision",
        "r2",
        "recall",
        "rmse",
        "rmsle",
        "roc_auc",
        "roc_index",
        "sensitivity",
        "specificity",
        "tnr",
        "tpr",
        "weighted_kappa2"
      ),
      check = function(x) throw(check_const_setting(x, "metrics"))
    ),

    metrics.ConfusionMatrix = list(
      value = c(
        "Accuracy" = "accuracy",
        "Kappa" = "kappa2",
        "Weighted Kappa" = "weighted_kappa2",
        "Sensitivity" = "sensitivity",
        "Specificity" = "specificity"
      ),
      check = check_metrics
    ),

    metrics.factor = list(
      value = c(
        "Brier" = "brier",
        "Accuracy" = "accuracy",
        "Kappa" = "kappa2",
        "Weighted Kappa" = "weighted_kappa2",
        "ROC AUC" = "roc_auc",
        "Sensitivity" = "sensitivity",
        "Specificity" = "specificity"
      ),
      check = check_metrics
    ),

    metrics.matrix = list(
      value = c(
        "RMSE" = "rmse",
        "R2" = "r2",
        "MAE" = "mae"
      ),
      check = check_metrics
    ),

    metrics.numeric = list(
      value = c(
        "RMSE" = "rmse",
        "R2" = "r2",
        "MAE" = "mae"
      ),
      check = check_metrics
    ),

    metrics.Surv = list(
      value = c(
        "C-Index" = "cindex",
        "Brier" = "brier",
        "ROC AUC" = "roc_auc",
        "Accuracy" = "accuracy"
      ),
      check = check_metrics
    ),

    models = list(
      value = c(
        "AdaBagModel",
        "AdaBoostModel",
        "BARTMachineModel",
        "BARTModel",
        "BlackBoostModel",
        "C50Model",
        "CForestModel",
        "CoxModel",
        "CoxStepAICModel",
        "EarthModel",
        "FDAModel",
        "GAMBoostModel",
        "GBMModel",
        "GLMBoostModel",
        "GLMModel",
        "GLMStepAICModel",
        "GLMNetModel",
        "KNNModel",
        "LARSModel",
        "LDAModel",
        "LMModel",
        "MDAModel",
        "NaiveBayesModel",
        "NNetModel",
        "ParsnipModel",
        "PDAModel",
        "PLSModel",
        "POLRModel",
        "QDAModel",
        "RandomForestModel",
        "RangerModel",
        "RFSRCModel",
        "RPartModel",
        "SelectedModel",
        "StackedModel",
        "SuperModel",
        "SurvRegModel",
        "SurvRegStepAICModel",
        "SVMModel",
        "SVMANOVAModel",
        "SVMBesselModel",
        "SVMLaplaceModel",
        "SVMLinearModel",
        "SVMPolyModel",
        "SVMRadialModel",
        "SVMSplineModel",
        "SVMTanhModel",
        "TreeModel",
        "TunedModel",
        "XGBModel",
        "XGBDARTModel",
        "XGBLinearModel",
        "XGBTreeModel"
      ),
      check = function(x) throw(check_const_setting(x, "models"))
    ),

    print_max = list(
      value = 10,
      check = function(x) {
        result <- check_numeric(x, bounds = c(1, Inf), size = 1)
        if (is.numeric(result)) result <- floor(result)
        result
      }
    ),

    require = list(
      value = "MachineShop",
      check = function(x) {
        x <- setdiff(x, .global_defaults$require)
        unavailable <- !vapply(x, requireNamespace, logical(1), quietly = TRUE)
        if (any(unavailable)) {
          DomainError(
            x, note_items("includes unavailable package{?s}: ", x[unavailable])
          )
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
          DomainError(x, "must be one or more character strings")
        }
      }
    ),

    response_types = list(
      value = c(
        "binary", "BinomialVariate", "DiscreteVariate", "factor", "matrix",
        "NegBinomialVariate", "numeric", "ordered", "PoissonVariate", "Surv"
      ),
      check = function(x) throw(check_const_setting(x, "response_types"))
    ),

    RHS.formula = list(
      value = sort(c(
        ".", "(", ":", "%in%", "I", "offset",
        "+", "-", "*", "/", "^", "%%", "%/%",
        "&", "|", "!",
        "==", "!=", "<", "<=", ">=", ">",
        "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round", "signif",
        "exp", "log", "expm1", "log1p", "cos", "sin", "tan", "cospi", "sinpi",
        "tanpi", "acos", "asin", "atan",
        "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
        "lgamma", "gamma", "digamma", "trigamma"
      )),
      check = function(x) throw(check_const_setting(x, "RHS.formula"))
    ),

    stat.Curve = list(
      value = c("Mean" = "base::mean"),
      check = function(x) {
        if (!is.null(x) && is(check_stat(x), "error")) {
          DomainError(
            x, "must be a statistics function, function name, vector of ",
            "these, or NULL"
          )
        } else x
      }
    ),

    stat.Resample = list(
      value = c("Mean" = "base::mean"),
      check = check_stat
    ),

    stat.TrainingParams = list(
      value = c("Mean" = "base::mean"),
      check = check_stat
    ),

    stats.PartialDependence = list(
      value = c("Mean" = "base::mean"),
      check = check_stats

    ),

    stats.Resample = list(
      value = c(
        "Mean" = "base::mean",
        "Median" = "stats::median",
        "SD" = "stats::sd",
        "Min" = "base::min",
        "Max" = "base::max"
      ),
      check = check_stats
    )

  ),

  throw_times = list()

))


.global_defaults <- settings()
