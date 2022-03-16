#' Confusion Matrix
#'
#' Calculate confusion matrices of predicted and observed responses.
#'
#' @name confusion
#' @rdname confusion
#'
#' @param x factor of \link[=response]{observed responses} or \link{resample}
#'   result containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param weights numeric vector of non-negative
#'   \link[=case_weights]{case weights} for the observed \code{x} responses
#'   [default: equal weights].
#' @param cutoff numeric (0, 1) threshold above which binary factor
#'   probabilities are classified as events and below which survival
#'   probabilities are classified.  If \code{NULL}, then factor responses are
#'   summed directly over predicted class probabilities, whereas a default
#'   cutoff of 0.5 is used for survival probabilities.  Class probability
#'   summations and survival will appear as decimal numbers that can be
#'   interpreted as expected counts.
#' @param na.rm logical indicating whether to remove observed or predicted
#'   responses that are \code{NA} when calculating metrics.
#' @param ... arguments passed to other methods.
#' @param data square matrix, or object that can be converted to one, of
#'   cross-classified predicted and observed values in the rows and columns,
#'   respectively.
#' @param ordered logical indicating whether the confusion matrix row and
#'   columns should be regarded as ordered.
#'
#' @return
#' The return value is a \code{ConfusionMatrix} class object that inherits from
#' \code{table} if \code{x} and \code{y} responses are specified or a
#' \code{ConfusionList} object that inherits from \code{list} if \code{x} is a
#' \code{Resample} object.
#'
#' @seealso \code{\link{c}}, \code{\link{plot}}, \code{\link{summary}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' (conf <- confusion(res))
#' plot(conf)
#' }
#'
confusion <- function(
  x, y = NULL, weights = NULL, cutoff = MachineShop::settings("cutoff"),
  na.rm = TRUE, ...
) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y, weights = weights)
    x <- complete$x
    y <- complete$y
    weights <- complete$weights
  }
  .confusion(x, y, weights, cutoff = cutoff)
}


#' @rdname confusion
#'
ConfusionMatrix <- function(data = NA, ordered = FALSE) {
  data <- as.matrix(data)

  n <- nrow(data)
  if (n != ncol(data)) throw(Error("Unequal number of rows and columns."))

  data_dimnames <- dimnames(data)
  if (is.null(data_dimnames)) data_dimnames <- list(NULL, NULL)
  names(data_dimnames) <- c("Predicted", "Observed")

  data_class <- "ConfusionMatrix"
  if (n == 2) data_class <- paste0("Binary", data_class)
  if (ordered) data_class <- paste0("Ordered", data_class)

  new(data_class, structure(data, dimnames = data_dimnames))
}


setGeneric(".confusion",
  function(observed, predicted, ...) standardGeneric(".confusion")
)


setMethod(".confusion", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    throw(Error(
      "Confusion requires a predicted factor or survival probabilities."
    ))
  }
)


setMethod(".confusion", c("factor", "factor"),
  function(observed, predicted, weights, ...) {
    weights <- check_weights(weights, observed)
    throw(check_assignment(weights))
    conf <- xtabs(weights ~ predicted + observed)
    ConfusionMatrix(conf, ordered = is.ordered(observed))
  }
)


setMethod(".confusion", c("factor", "matrix"),
  function(observed, predicted, weights, cutoff = NULL, ...) {
    if (is_empty(cutoff)) {
      weights <- check_weights(weights, observed)
      throw(check_assignment(weights))
      df <- aggregate(weights * predicted, list(observed), sum, na.rm = TRUE)
      conf <- as.table(t(df[, -1, drop = FALSE]))
      dimnames(conf) <- df[c(1, 1)]
      ConfusionMatrix(conf, ordered = is.ordered(observed))
    } else {
      predicted <- convert_response(observed, predicted)
      .confusion(observed, predicted, weights = weights, ...)
    }
  }
)


setMethod(".confusion", c("factor", "numeric"),
  function(observed, predicted, cutoff, ...) {
    predicted <- if (is_empty(cutoff)) {
      cbind(1 - predicted, predicted)
    } else {
      convert_response(observed, predicted, cutoff = cutoff)
    }
    .confusion(observed, predicted, ...)
  }
)


setMethod(".confusion", c("Resample", "ANY"),
  function(observed, predicted, weights, ...) {
    conf_list <- by(observed, observed$Model, function(resample) {
      confusion(resample$Observed, resample$Predicted, resample$Weight,
                na.rm = FALSE, ...)
    }, simplify = FALSE)
    if (all(map("logi", is, conf_list, "ConfusionList"))) {
      conf_list <- unlist(conf_list, recursive = FALSE)
    }
    do.call(c, conf_list)
  }
)


setMethod(".confusion", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    if (is_empty(cutoff)) cutoff <- 0.5
    predicted <- convert_response(observed, predicted, cutoff = cutoff)
    .confusion(observed, predicted, ...)
  }
)


setMethod(".confusion", c("Surv", "SurvEvents"),
  function(observed, predicted, weights, ...) {

    weights <- check_weights(weights, observed)
    throw(check_assignment(weights))
    times <- predicted@times
    conf_tbl <- table(Predicted = 0:1, Observed = 0:1)

    conf_list <- map(function(i) {
      pos <- predicted[, i, drop = TRUE] == 1
      pos_pred <- surv_subset(observed, weights, pos, times[i])
      neg_pred <- surv_subset(observed, weights, !pos, times[i])

      conf_tbl[1, 1] <- neg_pred$surv * neg_pred$p
      conf_tbl[2, 1] <- pos_pred$surv * pos_pred$p
      conf_tbl[1, 2] <- (1 - neg_pred$surv) * neg_pred$p
      conf_tbl[2, 2] <- (1 - pos_pred$surv) * pos_pred$p

      ConfusionMatrix(length(observed) * conf_tbl)
    }, seq_along(times))
    names(conf_list) <- colnames(predicted)
    do.call(c, conf_list)

  }
)
