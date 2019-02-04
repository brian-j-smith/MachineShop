#' Model Lift
#' 
#' Calculate lift estimates from observed and predicted responses.
#' 
#' @rdname lift
#' 
#' @param x observed responses or \code{Resamples} object of observed and
#' predicted responses.
#' @param y predicted responses.
#' @param times numeric vector of follow-up times if \code{y} contains predicted
#' survival probabilities.
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
lift <- function(x, y = NULL, times = numeric(), ...) {
  Lift(performance_curve(x, y = y, metrics = c(tpr, rpp), times = times))
}
