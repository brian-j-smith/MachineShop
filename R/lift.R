#' Model Lift
#' 
#' Calculate lift estimates from observed and predicted responses.
#' 
#' @name lift
#' @rdname lift
#' 
#' @param ... named or unnamed \code{lift} output to combine together with the
#' \code{Lift} constructor.
#' 
Lift <- function(...) {
  object <- as(Curves(...), "Lift")
  if (!all(mapply(identical, object@metrics, c(tpr, rpp)))) {
    stop("incorrect lift metrics")
  }
  object
}


#' @rdname lift
#' 
#' @param x observed responses or \code{Resamples} object of observed and
#' predicted responses.
#' @param y predicted responses.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating metrics.
#' 
#' @return \code{Lift} class object that inherits from \code{Curves}.
#'  
#' @seealso \code{\link{response}}, \code{\link{predict}},
#' \code{\link{resample}}, \code{\link{plot}}
#' 
#' @examples
#' library(MASS)
#' 
#' res <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#' lf <- lift(res)
#' plot(lf)
#' 
lift <- function(x, y = NULL, na.rm = TRUE, ...) {
  Lift(performance_curve(x, y = y, metrics = c(tpr, rpp), na.rm = na.rm))
}
