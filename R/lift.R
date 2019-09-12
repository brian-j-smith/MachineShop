#' Model Lift
#' 
#' Calculate lift estimates from observed and predicted responses.
#' 
#' @name lift
#' @rdname lift
#' 
#' @param x \link[=response]{observed responses} or \link{resample} result
#' containing observed and predicted responses.
#' @param y \link[=predict]{predicted responses} if not contained in \code{x}.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating metrics.
#' @param ... named or unnamed \code{lift} output to combine together with the
#' \code{Lift} constructor.
#' 
#' @return \code{Lift} class object that inherits from \code{Curves}.
#'  
#' @seealso \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' library(MASS)
#' 
#' res <- resample(type ~ ., data = Pima.tr, model = GBMModel)
#' lf <- lift(res)
#' plot(lf)
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
lift <- function(x, y = NULL, na.rm = TRUE, ...) {
  Lift(performance_curve(x, y = y, metrics = c(tpr, rpp), na.rm = na.rm))
}
