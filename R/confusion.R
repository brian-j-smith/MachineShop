#' Confusion Matrix
#' 
#' Calculate confusion matrices of predicted and observed responses.
#' 
#' @name confusion
#' @rdname confusion
#' 
#' @param x factor of \link[=response]{observed responses} or \link{resample}
#' result containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param cutoff numeric (0, 1) threshold above which binary factor
#' probabilities are classified as events and below which survival probabilities
#' are classified.
#' If \code{NULL}, then binary responses are summed directly over predicted
#' class probabilities, whereas a default cutoff of 0.5 is used for
#' survival probabilities.  Class probability summations and survival will
#' appear as decimal numbers that can be interpreted as expected counts.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating metrics.
#' @param ... named or unnamed \code{confusion} output to combine together with
#' the \code{Confusion} constructor.
#' @param object square matrix, or object that can be converted to one, of
#' cross-classified predicted and observed values in the rows and columns,
#' respectively.
#' @param ordered logical indicating whether the confusion matrix row and
#' columns should be regarded as ordered.
#' 
#' @return
#' The return value is a \code{ConfusionMatrix} class object that inherits from
#' \code{table} if \code{x} and \code{y} responses are specified or a
#' \code{Confusion} object that inherits from \code{list} if \code{x} is a
#' \code{Resamples} object.
#'  
#' @seealso \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' res <- resample(Species ~ ., data = iris, model = GBMModel)
#' (conf <- confusion(res))
#' plot(conf)
#' 
ConfusionMatrix <- function(object = numeric(), ordered = FALSE) {
  
  object <- if (length(object)) as.matrix(object) else matrix(NA_real_, 0, 0)
  
  n <- nrow(object)
  if (n != ncol(object)) stop("unequal number of rows and columns")

  object_dimnames <- dimnames(object)
  if (is.null(object_dimnames)) object_dimnames <- list(NULL, NULL)
  names(object_dimnames) <- c("Predicted", "Observed")
  
  object_class <- "ConfusionMatrix"
  if (n == 2) object_class <- paste0("Binary", object_class)
  if (ordered) object_class <- paste0("Ordered", object_class)

  new(object_class, structure(object, dimnames = object_dimnames))
}


#' @rdname confusion
#' 
Confusion <- function(...) {
  args <- list(...)
  
  conf_list <- list()
  for (i in seq(args)) {
    x <- args[[i]]
    if (is(x, "ConfusionMatrix")) {
      x <- list("Model" = x)
    } else if (!is(x, "Confusion")) {
      stop("values to combine must be Confusion or ConfusionMatrix objects")
    }
    arg_name <- names(args)[i]
    if (!is.null(arg_name) && nzchar(arg_name)) {
      names(x) <- rep(arg_name, length(x))
    }
    conf_list <- c(conf_list, x)
  }
  names(conf_list) <- make.unique(names(conf_list))

  structure(conf_list, class = c("Confusion", "listof"))
}


#' @rdname confusion
#' 
confusion <- function(x, y = NULL, cutoff = 0.5, na.rm = TRUE, ...) {
  if (na.rm) {
    complete <- complete_subset(x = x, y = y)
    x <- complete$x
    y <- complete$y
  }
  .confusion(x, y, cutoff = cutoff)
}


.confusion <- function(x, ...) {
  UseMethod(".confusion")
}


.confusion.default <- function(x, y, cutoff, ...) {
  if (!is.null(cutoff)) y <- convert_response(x, y, cutoff = cutoff)
  ConfusionMatrix(.confusion_matrix(x, y), ordered = is.ordered(x))
}


.confusion.Resamples <- function(x, cutoff, ...) {
  conf_list <- by(x, list(Model = x$Model), function(data) {
   confusion(data$Observed, data$Predicted, cutoff = cutoff, na.rm = FALSE)
  }, simplify = FALSE)
  if (all(mapply(is, conf_list, "Confusion"))) {
    conf_list <- unlist(conf_list, recursive = FALSE)
  }
  do.call(Confusion, conf_list)
}


.confusion.Surv <- function(x, y, cutoff, ...) {
  if (is.null(cutoff)) cutoff <- 0.5
  do.call(Confusion, .confusion_matrix(x, y, cutoff = cutoff))
}


setGeneric(".confusion_matrix", function(observed, predicted, ...)
  standardGeneric(".confusion_matrix"))


setMethod(".confusion_matrix", c("ANY", "ANY"),
  function(observed, predicted, ...) {
    stop("confusion requires a predicted factor or survival probabilities")
  }
)


setMethod(".confusion_matrix", c("factor", "factor"),
  function(observed, predicted, ...) {
    table(Predicted = predicted, Observed = observed)
  }
)


setMethod(".confusion_matrix", c("factor", "matrix"),
  function(observed, predicted, ...) {
    df <- aggregate(predicted, list(observed), sum, na.rm = TRUE)
    df[, -1, drop = FALSE] %>%
      t %>%
      as.table %>%
      structure(dimnames = list(Predicted = df[[1]], Observed = df[[1]]))
  }
)


setMethod(".confusion_matrix", c("factor", "numeric"),
  function(observed, predicted, ...) {
    .confusion_matrix(observed, cbind(1 - predicted, predicted))
  }
)


setMethod(".confusion_matrix", c("Surv", "SurvProbs"),
  function(observed, predicted, cutoff, ...) {
    predicted <- convert_response(observed, predicted, cutoff = cutoff)
    .confusion_matrix(observed, predicted)
  }
)


setMethod(".confusion_matrix", c("Surv", "SurvEvents"),
  function(observed, predicted, ...) {
    times <- time(predicted)
    surv <- predict(survfit(observed ~ 1, se.fit = FALSE), times)
    
    conf_tbl <- table(Predicted = 0:1, Observed = 0:1)

    structure(
      lapply(1:length(times), function(i) {
        surv_positives <- 1
        positives <- predicted[, i, drop = TRUE] == 1
        p <- mean(positives)
        if (p > 0) {
          obs <- observed[positives]
          valid_events <- obs[, "status"] == 1 & obs[, "time"] <= times[i]
          event_times <- sort(unique(obs[valid_events, "time"]))
          for (event_time in event_times) {
            d <- sum(obs[, "time"] == event_time & obs[, "status"] == 1)
            n <- sum(obs[, "time"] >= event_time)
            surv_positives <- surv_positives * (1 - d / n)
          }
        }

        conf_tbl[1, 1] <- surv[i] - surv_positives * p
        conf_tbl[2, 1] <- surv_positives * p
        conf_tbl[1, 2] <- 1 - p - conf_tbl[1, 1]
        conf_tbl[2, 2] <- p - surv_positives * p

        ConfusionMatrix(length(observed) * conf_tbl)
      }),
      names = paste0("time", seq_along(times))
    )
  }
)
