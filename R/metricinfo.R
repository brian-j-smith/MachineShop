#' Display Performance Metric Information
#' 
#' Display information about metrics provided by the \pkg{MachineShop} package.
#' 
#' @param ... one or more metric functions, function names, observed response,
#' observed and predicted responses, or a \code{Resamples} object.  If none are
#' specified, information is returned on all available metrics by default.
#' 
#' @return List of named metrics containing a descriptive \code{"label"},
#' whether to \code{"maximize"} the metric for better performance, the function
#' \code{"arguments"}, and supported response variable \code{"types"} for each.
#' 
#' @seealso \code{\link{metrics}}, \code{\link{resample}}
#' 
#' @examples
#' ## All metrics
#' metricinfo()
#' 
#' ## Metrics by observed and predicted response types
#' names(metricinfo(factor(0)))
#' names(metricinfo(factor(0), factor(0)))
#' names(metricinfo(factor(0), matrix(0)))
#' names(metricinfo(factor(0), numeric(0)))
#' 
metricinfo <- function(...) {
  args <- list(...)
  if (length(args) == 0) args <- as.list(.metric_names)
  info <- do.call(.metricinfo, args)
  
  is_type <- !sapply(info, is, class2 = "list")
  if (any(is_type)) {
    info_metrics <- if (all(is_type)) metricinfo() else info[!is_type]
    info_types <- do.call(.metricinfo_types, info[is_type])
    info <- c(info_metrics, info_types)
    info <- info[intersect(names(info_metrics), names(info_types))]
  }
  
  info[unique(names(info))]
}


.metric_names = c("accuracy",
                  "brier",
                  "cindex",
                  "cross_entropy",
                  "f_score",
                  "gini",
                  "kappa2",
                  "mae",
                  "mse",
                  "msle",
                  "npv",
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
                  "weighted_kappa2")


.metricinfo <- function(x, ...) {
  UseMethod(".metricinfo")
}


.metricinfo.default <- function(x, ...) {
  info <- list(x)
  if (length(list(...))) c(info, .metricinfo(...)) else info
}


.metricinfo.character <- function(x, ...) {
  metric <- try(get(x, mode = "function"), silent = TRUE)
  if (is(metric, "try-error")) metric <- list()
  .metricinfo(metric, ...)
}


.metricinfo.function <- function(x, ...) {
  .metricinfo(list(), ...)
}


.metricinfo.list <- function(x, ...) {
  if (length(list(...))) .metricinfo(...) else list()
}


.metricinfo.MLMetric <- function(x, ...) {
  generic_name <- paste0(".", x@name)
  if (isGeneric(generic_name)) {
    methods <- findMethods(generic_name)
    is_defined <- sapply(methods, function(method) {
      body(method) != quote(numeric())
    })
    types <- as.data.frame(do.call(rbind, methods@signatures[is_defined]),
                           stringsAsFactors = FALSE)
    names(types) <- methods@arguments
  } else {
    types <- NULL
  }
  
  info <- structure(list(list(
    label = x@label,
    maximize = x@maximize,
    arguments = args(x),
    types = types
  )), names = x@name)
  
  if (length(list(...))) c(info, .metricinfo(...)) else info
}


.metricinfo.Resamples <- function(x, ...) {
  .metricinfo(x$Observed, x$Predicted, ...)
}


.metricinfo_types <- function(x, y = NULL, ...) {
  info <- metricinfo()
  is_supported <- sapply(info, function(this) {
    is_method_type <- inherits(x, this$types$observed)
    if (is.null(y)) is_method_type else
      is_method_type && inherits(y, this$types$predicted)
  })
  info[is_supported]
}
