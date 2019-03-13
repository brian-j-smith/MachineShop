#' Performance Curves
#' 
#' Curves for the analysis of tradeoffs between metrics for assessing
#' performance in classifying binary outcomes over the range of possible
#' cutoff probabilities.  Available curves include receiver operating
#' characteristic (ROC) and precision recall.
#' 
#' @rdname performance_curve
#' 
#' @param x observed responses or \code{Resamples} object of observed and
#' predicted responses.
#' @param y predicted responses.
#' @param metrics list of two performance metrics for the analysis [default: ROC
#' metrics].  Precision recall curves can be obtained with 
#' \code{c(precision, recall)}.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating metrics.
#' 
#' @return \code{Curves} class object that inherits from \code{data.frame}.
#'  
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{metrics}}, \code{\link{auc}},
#' \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' library(MASS)
#' 
#' res <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#' 
#' ## ROC curve
#' roc <- performance_curve(res)
#' plot(roc)
#' auc(roc)
#' 
performance_curve <- function(x, ...) {
  UseMethod("performance_curve")
}


#' @rdname performance_curve
#' 
performance_curve.Resamples <- function(x, metrics = c(MachineShop::tpr,
                                                       MachineShop::fpr),
                                        na.rm = TRUE, ...) {
  metrics <- .get_curve_metrics(metrics)

  if (na.rm) x <- na.omit(x)
  
  curves <- NULL
  for (model in unique(x$Model)) {
    for (resample in unique(x$Resample)) {
      df <- subset(x, Model == model & Resample == resample)
      curve <- .curve_default(df$Observed, df$Predicted, metrics = metrics)
      curve <- if (is(curve, "listof")) {
        structure(curve, names = paste0(model, ".", names(curve)))
      } else {
        structure(list(curve), names = model)
      }
      curve <- do.call(Curves, curve)
      curve$Resample <- resample
      curves <- rbind(curves, curve)
    }
  }

  new("Curves", curves, metrics = curve@metrics)
}


#' @rdname performance_curve
#' 
performance_curve.default <- function(x, y, metrics = c(MachineShop::tpr,
                                                        MachineShop::fpr),
                                      ...) {
  .curve(x, y, metrics = .get_curve_metrics(metrics))
}


.get_curve_metrics <- function(metrics) {
  if (is.character(metrics)) {
    metrics <- mget(metrics, mode = "function", inherits = TRUE)
  }
  
  if (length(metrics) != 2 || !all(mapply(is, metrics, "MLMetric"))) {
    stop("'metrics' must be a list of two performance metrics")
  }
  
  metrics
}


.curve <- function(x, ...) {
  UseMethod(".curve")
}


.curve.default <- function(x, y, metrics, stat, ...) {
  .curve_default(x, y, metrics = metrics)
}


.curve.Surv <- function(x, y, metrics, stat, ...) {
  do.call(Curves, .curve_default(x, y, metrics = metrics))
}


setGeneric(".curve_default", function(observed, predicted, ...)
  standardGeneric(".curve_default"))


setMethod(".curve_default", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("performance_curve requires a predicted binary factor or survival",
         " probabilities")
  }
)


setMethod(".curve_default", c("factor", "numeric"),
  function(observed, predicted, metrics, ...) {
    cutoffs <- c(-Inf, unique(predicted))
    x <- y <- numeric(length(cutoffs))
    for (i in 1:length(cutoffs)) {
      conf <- confusion(observed, predicted, cutoff = cutoffs[i])
      x[i] <- metrics[[2]](conf)
      y[i] <- metrics[[1]](conf)
    }
    Curves(data.frame(Cutoff = cutoffs, x = x, y = y), .metrics = metrics)
  }
)


setMethod(".curve_default", c("Surv", "SurvProbs"),
  function(observed, predicted, metrics, ...) {
    times <- predicted@times
    surv <- predict(survfit(observed ~ 1, se.fit = FALSE), times)

    conf <- ConfusionMatrix(table(Predicted = 0:1, Observed = 0:1))
    
    structure(
      lapply(1:length(times), function(i) {
        
        time <- times[i]
        surv_all <- surv[i]
        pred <- predicted[, i, drop = TRUE]
        
        cutoffs <- c(-Inf, unique(pred))
        x <- y <- numeric(length(cutoffs))
        for (j in 1:length(cutoffs)) {
          surv_positives <- 1
          positives <- pred <= cutoffs[j]
          p <- mean(positives)
          if (p > 0) {
            obs <- observed[positives]
            valid_events <- obs[, "status"] == 1 & obs[, "time"] <= time
            event_times <- sort(unique(obs[valid_events, "time"]))
            for (event_time in event_times) {
              d <- sum(obs[, "time"] == event_time & obs[, "status"] == 1)
              n <- sum(obs[, "time"] >= event_time)
              surv_positives <- surv_positives * (1 - d / n)
            }
          }
          
          conf[1, 1] <- surv_all - surv_positives * p
          conf[2, 1] <- surv_positives * p
          conf[1, 2] <- 1 - p - conf[1, 1]
          conf[2, 2] <- p - surv_positives * p
          
          x[j] <- metrics[[2]](conf)
          y[j] <- metrics[[1]](conf)
        }
        
        Curves(data.frame(Cutoff = cutoffs, x = x, y = y), .metrics = metrics)
        
      }),
      class = "listof",
      names = paste0("time", seq_along(times))
    )
  }
)
