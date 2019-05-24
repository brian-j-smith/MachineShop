#################### Metric Method Functions ####################


setMetric_auc <- function(f, metrics) {
  definition <- function(observed, predicted, ...) {
    auc(observed, predicted, metrics = metrics)
  }
  setMetricGeneric(f)
  setMetricMethod(f, c("factor", "factor"))
  setMetricMethod(f, c("factor", "numeric"), definition)
  setMetricMethod_Resamples(f)
  setMetricMethod(f, c("Surv", "SurvProbs"), definition)
}


setMetric_BinaryConfusionMatrix <- function(f, definition) {
  setMetricGeneric(f)
  setMetricMethod(f, c("BinaryConfusionMatrix", "NULL"), definition)
  setMetricMethod_factor_factor(f)  
  setMetricMethod_factor_numeric(f)
  setMetricMethod_Resamples(f)
  setMetricMethod_Surv_SurvEvents(f)
  setMetricMethod_Surv_SurvProbs(f)  
}


setMetric_ConfusionMatrix <- function(f, definition) {
  setMetricGeneric(f)
  setMetricMethod(f, c("ConfusionMatrix", "NULL"), definition)
  setMetricMethod_factor_factor(f)  
  setMetricMethod_factor_matrix(f)
  setMetricMethod_factor_numeric(f)
  setMetricMethod_Resamples(f)
  setMetricMethod_Surv_SurvEvents(f)
  setMetricMethod_Surv_SurvProbs(f)  
}


setMetric_numeric <- function(f, definition) {
  setMetricGeneric(f)
  setMetricMethod(f, c("numeric", "numeric"), definition)
  setMetricMethod_matrix_matrix(f)
  setMetricMethod_Resamples(f)
  setMetricMethod_Surv_numeric(f)
}


setMetricGeneric <- function(f) {
  
  eval(substitute(
    setGeneric(name, function(observed, predicted, ...) standardGeneric(name)),
    list(name = metric_method_name(f))
  ))
  
  setMetricMethod(f, c("ANY", "ANY"))
  
}


setMetricMethod <- function(f, signature, definition =
                              function(observed, predicted, ...) numeric()) {
  setMethod(metric_method_name(f), signature, definition)
}


setMetricMethod_factor_factor <- function(f) {
  setMetricMethod(f, c("factor", "factor"),
    function(observed, predicted, ...) {
      get(f)(confusion(observed, predicted), ...)
    }
  )
}


setMetricMethod_factor_matrix <- function(f) {
  setMetricMethod(f, c("factor", "matrix"),
    function(observed, predicted, ...) {
      get(f)(confusion(observed, predicted), ...)
    }
  )
}


setMetricMethod_factor_numeric <- function(f) {
  setMetricMethod(f, c("factor", "numeric"),
    function(observed, predicted, cutoff, ...) {
      get(f)(confusion(observed, predicted, cutoff = cutoff), ...)
    }
  )
}


setMetricMethod_matrix_matrix <- function(f) {
  setMetricMethod(f, c("matrix", "matrix"),
    function(observed, predicted, ...) {
      metric_matrix(observed, predicted, get(f), ...)
    }
  )
}


setMetricMethod_Resamples <- function(f) {
  setMetricMethod(f, c("Resamples", "NULL"),
    function(observed, predicted, ...) {
      performance(observed, metrics = get(f), ...)
    }
  )
}


setMetricMethod_Surv_numeric <- function(f) {
  setMetricMethod(f, c("Surv", "numeric"),
    function(observed, predicted, ...) {
      metric_SurvMean(observed, predicted, get(f), ...)
    }
  )
}


setMetricMethod_Surv_SurvEvents <- function(f) {
  setMetricMethod(f, c("Surv", "SurvEvents"),
    function(observed, predicted, ...) {
      metric_SurvMatrix(observed, predicted, get(f), ...)
    }
  )
}


setMetricMethod_Surv_SurvProbs <- function(f) {
  setMetricMethod(f, c("Surv", "SurvProbs"),
    function(observed, predicted, ...) {
      metric_SurvMatrix(observed, predicted, get(f), ...)
    }
  )
}


#################### Utility Functions ####################


call_metric_method <- function(f, envir) {
  do.call(metric_method_name(f), as.list(envir))
}


metric_method_name <- function(f) {
  paste0(".", f)
}


metric_matrix <- function(observed, predicted, FUN, ...) {
  mean(sapply(1:ncol(observed), function(i) {
    FUN(observed[, i], predicted[, i], ...)
  }))
}


metric_SurvMatrix <- function(observed, predicted, FUN, cutoff = NULL, ...) {
  conf <- confusion(observed, predicted, cutoff = cutoff)
  x <- sapply(conf, FUN, ...)
  times <- time(predicted)
  if (length(times) > 1) c("mean" = surv_metric_mean(x, times), x) else x[[1]]
}


metric_SurvMean <- function(observed, predicted, FUN, ...) {
  events <- observed[, "status"] == 1
  FUN(observed[events, "time"], predicted[events], ...)
}
