#' Model Lift Curves
#'
#' Calculate lift curves from observed and predicted responses.
#'
#' @name lift
#' @rdname lift
#'
#' @param x \link[=response]{observed responses} or \link{resample} result
#'   containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param na.rm logical indicating whether to remove observed or predicted
#'   responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed to other methods.
#'
#' @return \code{LiftCurve} class object that inherits from
#' \code{PerformanceCurve}.
#'
#' @seealso \code{\link{c}}, \code{\link{plot}}, \code{\link{summary}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' data(Pima.tr, package = "MASS")
#'
#' res <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#' lf <- lift(res)
#' plot(lf)
#' }
#'
lift <- function(x, y = NULL, na.rm = TRUE, ...) {
  as(performance_curve(x, y = y, metrics = c(tpr, rpp), na.rm = na.rm),
     "LiftCurve")
}


LiftCurve <- function(...) {
  object <- as(PerformanceCurve(...), "LiftCurve")
  if (!all(map_logi(identical, object@metrics, c(tpr, rpp)))) {
    stop("incorrect LiftCurve metrics")
  }
  object
}


#' Model Performance Curves
#'
#' Calculate curves for the analysis of tradeoffs between metrics for assessing
#' performance in classifying binary outcomes over the range of possible
#' cutoff probabilities.  Available curves include receiver operating
#' characteristic (ROC) and precision recall.
#'
#' @name performance_curve
#' @aliases curves
#'
#' @param x \link[=response]{observed responses} or \link{resample} result
#'   containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param metrics list of two performance \link{metrics} for the analysis
#'   [default: ROC metrics].  Precision recall curves can be obtained with
#'   \code{c(precision, recall)}.
#' @param na.rm logical indicating whether to remove observed or predicted
#'   responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed to other methods.
#'
#' @return \code{PerformanceCurve} class object that inherits from
#' \code{data.frame}.
#'
#' @seealso \code{\link{auc}}, \code{\link{c}}, \code{\link{plot}},
#' \code{\link{summary}}
#'
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' data(Pima.tr, package = "MASS")
#'
#' res <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#'
#' ## ROC curve
#' roc <- performance_curve(res)
#' plot(roc)
#' auc(roc)
#' }
#'
performance_curve <- function(x, ...) {
  UseMethod("performance_curve")
}


#' @rdname performance_curve
#'
performance_curve.default <- function(
  x, y, metrics = c(MachineShop::tpr, MachineShop::fpr), na.rm = TRUE, ...
) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y)
    x <- complete$x
    y <- complete$y
  }
  .curve(x, y, metrics = .get_curve_metrics(metrics))
}


#' @rdname performance_curve
#'
performance_curve.Resamples <- function(
  x, metrics = c(MachineShop::tpr, MachineShop::fpr), na.rm = TRUE, ...
) {
  metrics <- .get_curve_metrics(metrics)

  if (na.rm) x <- na.omit(x)

  curves <- NULL
  for (model in unique(x$Model)) {
    for (resample in unique(x$Resample)) {
      df <- x[x$Model == model & x$Resample == resample, ]
      curve <- .curve_default(df$Observed, df$Predicted, metrics = metrics)
      curve <- if (is(curve, "listof")) {
        structure(curve, names = paste0(model, ".", names(curve)))
      } else {
        structure(list(curve), names = model)
      }
      curve <- do.call(c, curve)
      curve$Resample <- resample
      curves <- rbind(curves, curve)
    }
  }

  PerformanceCurve(curves, metrics = curve@metrics)
}


.get_curve_metrics <- function(metrics) {
  metrics <- map(fget, metrics)
  if (length(metrics) != 2 || !all(map_logi(is, metrics, "MLMetric"))) {
    stop("'metrics' must be a list of two performance metrics")
  }
  metrics
}


PerformanceCurve <- function(object, ..., metrics, .check = TRUE) {
  if (.check) {
    if (is.null(object$Model)) object$Model <- factor("Model")
    missing <- missing_names(c("Cutoff", "x", "y"), object)
    if (length(missing)) {
      stop(label_items("missing performance curve variable", missing))
    }

    if (!all(map_logi(is, metrics[1:2], "MLMetric"))) {
      stop("missing performance metrics in PerformanceCurve constructor")
    }
    metrics <- c(y = metrics[[1]], x = metrics[[2]])

    decreasing <- !xor(metrics$x@maximize, metrics$y@maximize)
    sort_order <- order(object$Model, object$x, object$y,
                        decreasing = c(FALSE, FALSE, decreasing),
                        method = "radix")
    object <- object[sort_order, , drop = FALSE]
  }

  rownames(object) <- NULL
  new("PerformanceCurve", object, metrics = metrics, ...)
}


.curve <- function(x, ...) {
  UseMethod(".curve")
}


.curve.default <- function(x, y, metrics, ...) {
  .curve_default(x, y, metrics = metrics)
}


.curve.Surv <- function(x, y, metrics, ...) {
  do.call(c, .curve_default(x, y, metrics = metrics))
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
    PerformanceCurve(data.frame(Cutoff = cutoffs, x = x, y = y),
                     metrics = metrics)
  }
)


setMethod(".curve_default", c("Surv", "SurvProbs"),
  function(observed, predicted, metrics, ...) {

    times <- predicted@times
    conf <- ConfusionMatrix(table(Predicted = 0:1, Observed = 0:1))

    structure(
      map(function(i) {
        time <- times[i]
        pred <- predicted[, i, drop = TRUE]
        cutoffs <- c(-Inf, unique(pred))
        x <- y <- numeric(length(cutoffs))

        for (j in 1:length(cutoffs)) {
          pos <- pred <= cutoffs[j]
          pos_pred <- surv_subset(observed, pos, time)
          neg_pred <- surv_subset(observed, !pos, time)

          conf[1, 1] <- neg_pred$surv * neg_pred$p
          conf[2, 1] <- pos_pred$surv * pos_pred$p
          conf[1, 2] <- (1 - neg_pred$surv) * neg_pred$p
          conf[2, 2] <- (1 - pos_pred$surv) * pos_pred$p

          x[j] <- metrics[[2]](conf)
          y[j] <- metrics[[1]](conf)
        }

        PerformanceCurve(data.frame(Cutoff = cutoffs, x = x, y = y),
                         metrics = metrics)
      }, 1:length(times)),
      class = "listof",
      names = paste0("time", seq_along(times))
    )

  }
)
