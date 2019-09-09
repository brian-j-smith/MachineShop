#' Display Performance Metric Information
#' 
#' Display information about metrics provided by the \pkg{MachineShop} package.
#' 
#' @param ... \link[=metrics]{metric} functions or function names;
#' \link[=response]{observed responses}; \link[=response]{observed} and
#' \link[=predict]{predicted} responses; \link{confusion} or \link{resample}
#' results; or vector of these for which to display information.  If none are
#' specified, information is returned on all available metrics by default.
#' 
#' @return List of named metric elements each containing the following
#' components:
#' \describe{
#' \item{label}{character descriptor for the metric.}
#' \item{maximize}{logical indicating whether higher values of the metric
#' correspond to better predictive performance.}
#' \item{arguments}{closure with the argument names and corresponding default
#' values of the metric function.}
#' \item{response_types}{data frame of the observed and predicted response
#' variable types supported by the metric.}
#' }
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
#' ## Metric-specific information
#' metricinfo(auc)
#' 
metricinfo <- function(...) {
  args <- list(...)
  if (length(args) == 1 && is.vector(args[[1]])) args <- as.list(args[[1]])
  args <- if (length(args)) unname(args) else as.list(.metric_names)
  info <- do.call(.metricinfo, args)
  
  is_type <- if (length(info)) !mapply(is, info, "list") else NULL
  if (any(is_type)) {
    info_metrics <- if (all(is_type)) metricinfo() else info[!is_type]
    info_types <- do.call(.metricinfo_types, info[is_type])
    info <- c(info_metrics, info_types)
    info <- info[intersect(names(info_metrics), names(info_types))]
  }
  
  info[unique(names(info))]
}


.metric_names = c("accuracy",
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
                  "ppv",
                  "pr_auc",
                  "precision",
                  "r2",
                  "recall",
                  "rmse",
                  "rmsle",
                  "roc_auc",
                  "roc_index",
                  "rpp",
                  "sensitivity",
                  "specificity",
                  "tnr",
                  "tpr",
                  "weighted_kappa2")


.metricinfo <- function(x, ...) {
  UseMethod(".metricinfo")
}


.metricinfo.default <- function(x, ...) {
  info <- list(x)
  if (length(list(...))) c(info, .metricinfo(...)) else info
}


.metricinfo.character <- function(x, ...) {
  metric <- try(fget(x), silent = TRUE)
  if (is(metric, "try-error")) metric <- list()
  .metricinfo(metric, ...)
}


.metricinfo.Confusion <- function(x, ...) {
  .metricinfo(x[[1]], ...)
}


.metricinfo.ConfusionMatrix <- function(x, ...) {
  c(list(x), .metricinfo(NULL, ...))
}


.metricinfo.function <- function(x, ...) {
  .metricinfo(list(), ...)
}


.metricinfo.list <- function(x, ...) {
  if (length(list(...))) .metricinfo(...) else list()
}


.metricinfo.MLMetric <- function(x, ...) {
  generic_name <- x@name
  if (!isGeneric(generic_name)) generic_name <- paste0(".", x@name)
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
    response_types = types
  )), names = x@name)
  
  if (length(list(...))) c(info, .metricinfo(...)) else info
}


.metricinfo.Resamples <- function(x, ...) {
  .metricinfo(x$Observed, x$Predicted, ...)
}


.metricinfo_types <- function(x, y, ...) {
  not_missing_y <- !missing(y)
  info <- metricinfo()
  is_supported <- sapply(info, function(this) {
    is_types <- mapply(is, list(x), this$response_types$observed)
    if (not_missing_y) {
      is_types <- is_types & mapply(is, list(y), this$response_types$predicted)
    }
    any(is_types)
  })
  info[is_supported]
}
