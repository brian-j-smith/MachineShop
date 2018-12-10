#' Model Lift
#' 
#' Calculate lift estimates from observed and predicted responses.
#' 
#' @rdname lift
#' 
#' @param x observed responses or \code{Resamples} object of observed and
#' predicted responses.
#' @param y predicted responses.
#' 
#' @return \code{Lift} class object that inherits from \code{data.frame}.
#'  
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{plot}}
#' 
#' @examples
#' library(MASS)
#' 
#' res <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#' (lf <- lift(res))
#' plot(lf)
#' 
lift <- function(x, y = NULL, ...) {
  .lift(x, y)
}


.lift <- function(x, ...) {
  UseMethod(".lift")
}


.lift.default <- function(x, y, ...) {
  Lift(.lift_default(x, y))
}


.lift.Resamples <- function(x, ...) {
  lf_list <- by(x, x$Model, function(data) {
    lift(data$Observed, data$Predicted)
  }, simplify = FALSE)
  do.call(Lift, lf_list)
}


setGeneric(".lift_default", function(observed, predicted)
  standardGeneric(".lift_default"))


setMethod(".lift_default", c("ANY", "ANY"),
  function(observed, predicted) stop("lift requires a binary response variable")
)


setMethod(".lift_default", c("factor", "numeric"),
  function(observed, predicted) {
    df <- data.frame(
      observed = observed == levels(observed)[2],
      predicted = predicted
    ) %>% na.omit
    order_indices <- order(df$predicted, decreasing = TRUE)
    observed <- df$observed[order_indices]
    data.frame(
      Found = 100 * c(0, cumsum(observed) / sum(observed)),
      Tested = 100 * c(0, seq(observed) / length(observed))
    )
  }
)
