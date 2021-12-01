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
#' @param weights numeric vector of non-negative
#'   \link[=case_weights]{case weights} for the observed \code{x} responses
#'   [default: equal weights].
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
lift <- function(x, y = NULL, weights = NULL, na.rm = TRUE, ...) {
  as(performance_curve(x, y, weights, metrics = c(tpr, rpp), na.rm = na.rm),
     "LiftCurve")
}


LiftCurve <- function(...) {
  object <- as(PerformanceCurve(...), "LiftCurve")
  if (!all(map("logi", identical, object@metrics, c(tpr, rpp)))) {
    throw(Error("Incorrect LiftCurve metrics."))
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
#' @param weights numeric vector of non-negative
#'   \link[=case_weights]{case weights} for the observed \code{x} responses
#'   [default: equal weights].
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
  x, y, weights = NULL, metrics = c(MachineShop::tpr, MachineShop::fpr),
  na.rm = TRUE, ...
) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y, weights = weights)
    x <- complete$x
    y <- complete$y
    weights <- complete$weights
  }
  metrics <- get_curve_metrics(metrics)
  curve <- .performance_curve(x, y, weights, metrics = metrics)
  if (is(curve, "listof")) do.call(c, curve) else curve
}


#' @rdname performance_curve
#'
performance_curve.Resample <- function(
  x, metrics = c(MachineShop::tpr, MachineShop::fpr), na.rm = TRUE, ...
) {
  if (na.rm) x <- na.omit(x)
  metrics <- get_curve_metrics(metrics)

  curves <- NULL
  for (model in unique(x$Model)) {
    for (iter in unique(x$Iteration)) {
      df <- x[x$Model == model & x$Iteration == iter, ]
      curve <- .performance_curve(df$Observed, df$Predicted, df$Weight,
                                  metrics = metrics)
      curve <- if (is(curve, "listof")) {
        structure(curve, names = paste0(model, ".", names(curve)))
      } else {
        structure(list(curve), names = model)
      }
      curve <- do.call(c, curve)
      curve$Iteration <- iter
      curves <- rbind(curves, curve)
    }
  }

  PerformanceCurve(curves, metrics = curve@metrics)
}


get_curve_metrics <- function(metrics) {
  metrics <- map(fget, metrics)
  if (length(metrics) != 2 || !all(map("logi", is, metrics, "MLMetric"))) {
    metrics <- Error("Value must be a list of two performance metrics.")
    throw(check_assignment(metrics))
  }
  metrics
}


PerformanceCurve <- function(object, ..., metrics, .check = TRUE) {
  if (.check) {
    if (is.null(object$Model)) object$Model <- factor("Model")
    missing <- missing_names(c("Cutoff", "x", "y"), object)
    if (length(missing)) {
      throw(Error(note_items(
        "Missing performance curve variable{?s}: ", missing, "."
      )))
    }

    if (!all(map("logi", is, metrics[1:2], "MLMetric"))) {
      throw(Error(
        "Missing performance metrics in PerformanceCurve constructor."
      ))
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


setGeneric(".performance_curve",
  function(observed, predicted, ...) standardGeneric(".performance_curve")
)


setMethod(".performance_curve", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    throw(Error("Performance_curve requires a predicted binary factor or ",
                "survival probabilities."))
  }
)


setMethod(".performance_curve", c("factor", "numeric"),
  function(observed, predicted, weights, metrics, ...) {
    cutoffs <- c(-Inf, unique(predicted))
    x <- y <- numeric(length(cutoffs))
    for (i in seq_along(cutoffs)) {
      conf <- confusion(observed, predicted, weights, cutoff = cutoffs[i])
      x[i] <- metrics[[2]](conf)
      y[i] <- metrics[[1]](conf)
    }
    PerformanceCurve(data.frame(Cutoff = cutoffs, x = x, y = y),
                     metrics = metrics)
  }
)


setMethod(".performance_curve", c("Surv", "SurvProbs"),
  function(observed, predicted, weights, metrics, ...) {

    weights <- check_weights(weights, observed)
    throw(check_assignment(weights))

    times <- predicted@times
    conf <- ConfusionMatrix(table(Predicted = 0:1, Observed = 0:1))

    structure(
      map(function(i) {
        time <- times[i]
        pred <- predicted[, i, drop = TRUE]
        cutoffs <- c(-Inf, unique(pred))
        x <- y <- numeric(length(cutoffs))

        for (j in seq_along(cutoffs)) {
          pos <- pred <= cutoffs[j]
          pos_pred <- surv_subset(observed, weights, pos, time)
          neg_pred <- surv_subset(observed, weights, !pos, time)

          conf[1, 1] <- neg_pred$surv * neg_pred$p
          conf[2, 1] <- pos_pred$surv * pos_pred$p
          conf[1, 2] <- (1 - neg_pred$surv) * neg_pred$p
          conf[2, 2] <- (1 - pos_pred$surv) * pos_pred$p

          x[j] <- metrics[[2]](conf)
          y[j] <- metrics[[1]](conf)
        }

        PerformanceCurve(data.frame(Cutoff = cutoffs, x = x, y = y),
                         metrics = metrics)
      }, seq_along(times)),
      names = make_names_len(length(times), "time"),
      class = "listof"
    )

  }
)
