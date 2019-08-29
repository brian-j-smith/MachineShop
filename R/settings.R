#' MachineShop Settings 
#' 
#' Allow the user to view or change global settings which affect default
#' behaviors of functions in the the \pkg{MachineShop} package.
#' 
#' @param ... character names of settings to view, \code{name = value} pairs
#' giving the values of settings to change, a vector of these, or no arguments
#' to view all settings.  Partial matching of setting names is supported.
#' 
#' @return The setting value if only one is specified to view.  Otherwise, a
#' list of the values of specified settings as they existed prior to any
#' requested changes.  Such a list can be passed as an argument to
#' \code{settings} to restore their values.
#' 
#' @section Settings:
#' 
#' \describe{
#'   \item{\code{control}}{\code{\link{MLControl}} object, control function, or
#'   character string naming a control function defining a default resampling
#'   method [default: \code{"CVControl"}].}
#'   \item{\code{metrics.ConfusionMatrix}}{function, function name, or vector of
#'   these with which to calculate \link{performance} \link{metrics} for
#'   confusion matrices [default: \code{c(Accuracy = "accuracy", Kappa =
#'   "kappa2", `Weighted Kappa` = "weighted_kappa2", Sensitivity =
#'   "sensitivity", Specificity = "specificity")}].}
#'   \item{\code{metrics.factor}}{function, function name, or vector of these
#'   with which to calculate \link{performance} \link{metrics} for factor
#'   responses [default: \code{c(Brier = "brier", Accuracy = "accuracy", Kappa =
#'   "kappa2", `Weighted Kappa` = "weighted_kappa2", `ROC AUC` = "roc_auc",
#'   Sensitivity = "sensitivity", Specificity = "specificity")}].}
#'   \item{\code{metrics.matrix}}{function, function name, or vector of these
#'   with which to calculate \link{performance} \link{metrics} for matrix
#'   responses [default: \code{c(RMSE = "rmse", R2 = "r2", MAE = "mae")}].}
#'   \item{\code{metrics.numeric}}{function, function name, or vector of these
#'   with which to calculate \link{performance} \link{metrics} for numeric
#'   responses [default: \code{c(RMSE = "rmse", R2 = "r2", MAE = "mae")}].}
#'   \item{\code{metrics.Surv}}{function, function name, or vector of these with
#'   which to calculate \link{performance} \link{metrics} for survival responses
#'   [default: \code{c(`C-Index` = "cindex", Brier = "brier", `ROC AUC` =
#'   "roc_auc", Accuracy = "accuracy")}].}
#' }
#' 
settings <- function(...) {
  
  args <- list(...)
  if(length(args) == 1 && is.null(names(args)) && is.vector(args[[1]])) {
    args <- args[[1]]
  }
  
  global_settings <- MachineShop_global$settings
  global_values <- lapply(global_settings, getElement, name = "value")
  global_checks <- lapply(global_settings, getElement, name = "check")
  
  if (!length(args)) return(global_values)
  
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


check_metrics <- function(x) {
  result <- try(lapply(c(x), getMLObject, class = "MLMetric"), silent = TRUE)
  if (is(result, "try-error")) {
    DomainError(x, "must be a metrics function, function name, ",
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
    )
    
  )
  
))
