#' Display Performance Metric Information
#' 
#' Display information about metrics provided by the \pkg{MachineShop} package.
#' 
#' @param ... one or more metric function names, observed response, observed and
#' predicted responses, or a \code{Resamples} object.  If none are specified,
#' information is returned on all available metrics by default.
#' 
#' @return List of named metrics available for the supplied arguments and
#' containing a descriptive \code{"label"}, the functions' \code{"arguments"},
#' and supported response variable \code{"types"}.
#' 
#' @seealso \code{\link{metrics}}, \code{\link{Resamples}}
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
  if (length(args) == 0) args <- list(NULL)
  do.call(.metricinfo, args)
}


setGeneric(".metricinfo", function(x, ...) standardGeneric(".metricinfo"))


setMethod(".metricinfo", "NULL",
  function(x, ...) {
    metric_labels = c("accuracy" = "Accuracy",
                      "brier" = "Brier Score",
                      "cindex" = "Concordance Index",
                      "cross_entropy" = "Cross Entropy",
                      "f_score" = "F Score",
                      "kappa" = "Cohen's Kappa",
                      "mae" = "Mean Absolute Error",
                      "mse" = "Mean Squared Error",
                      "npv" = "Negative Predictive Value",
                      "ppv" = "Positive Predictive Value",
                      "pr_auc" = "Area Under Precision-Recall Curve",
                      "precision" = "Precision",
                      "r2" = "Coefficient of Determination",
                      "recall" = "Recall",
                      "roc_auc" = "Area Under ROC Curve",
                      "roc_index" = "ROC Index",
                      "sensitivity" = "Sensitivity",
                      "specificity" = "Specificity",
                      "weighted_kappa" = "Weighted Cohen's Kappa")
    
    info <- list()
    for (name in names(metric_labels)) {
      methods <- findMethods(paste0(".", name))
      is_defined <- sapply(methods, function(x) body(x) != quote(numeric()))
      types <- as.data.frame(do.call(rbind, methods@signatures[is_defined]))
      names(types) <- methods@arguments
      
      info[[name]] <- list(
        label = metric_labels[[name]],
        arguments = args(get(name, mode = "function")),
        types = types
      )
      signatures <- methods@signatures
      structure(as.data.frame(do.call(rbind, methods@signatures)),
                names = methods@arguments)
    }
    info
  }
)


setMethod(".metricinfo", "ANY",
  function(x, y = NULL, ...) {
    info <- metricinfo()
    is_type <- sapply(info, function(this) {
      any(apply(this$type, 1, function(type) {
        is_method_type <- is(x, type[1])
        if (is.null(y)) is_method_type else is_method_type && is(y, type[2])
      }))
    })
    info[is_type]
  }
)


setMethod(".metricinfo", "character",
  function(x, ...) {
    metricinfo()[unique(c(x, ...))]
  }
)


setMethod(".metricinfo", "Resamples",
  function(x, ...) {
    metricinfo(x$Observed, x$Predicted)
  }
)
