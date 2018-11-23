#' Model Lift
#' 
#' Calculate lift estimates from observed and resampled response variable
#' values.
#' 
#' @param x \code{Resamples} object.
#' @param ... arguments passed to other methods.
#' 
#' @return \code{LiftResamples} class object that inherits from
#' \code{data.frame}.
#'  
#' @seealso \code{\link{resample}}, \code{\link{plot}}
#' 
#' @examples
#' library(MASS)
#' 
#' perf <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#' (lf <- lift(perf))
#' plot(lf)
#' 
lift <- function(x, ...) {
  stopifnot(is(x, "Resamples"))
  
  lift_list <- by(response(x), response(x)$Model, function(data) {
    .lift(data$Observed, data$Predicted) %>%
      cbind(Model = data$Model[1])
  }, simplify = FALSE)
  
  structure(do.call(rbind, lift_list), class = c("LiftResamples", "data.frame"))
}


setGeneric(".lift", function(observed, predicted) standardGeneric(".lift"))


setMethod(".lift", c("ANY", "ANY"),
  function(observed, predicted) stop("lift unavailable for response type")
)


setMethod(".lift", c("factor", "numeric"),
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
