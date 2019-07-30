#' MLMetric Class Constructor
#' 
#' Create a performance metric for use with the \pkg{MachineShop} package.
#' 
#' @rdname MLMetric
#' 
#' @param object function to compute the metric, defined to accept
#' \code{observed} and \code{predicted} as the first two arguments and with an
#' ellipsis (\code{...}) to accommodate others.
#' @param name character name of the object to which the metric is assigned.
#' @param label optional character descriptor for the model.
#' @param maximize logical indicating whether higher values of the metric
#' correspond to better predictive performance.
#' 
#' @return \code{MLMetric} class object.
#' 
#' @seealso \code{\link{metrics}}
#' 
MLMetric <- function(object, name = "MLMetric", label = name, maximize = TRUE) {
  new("MLMetric", object, name = name, label = label, maximize = maximize)
}


#' @rdname MLMetric
#' 
#' @param value list of arguments to pass to the \code{MLMetric} constructor.
#' 
#' @examples
#' f2_score <- function(observed, predicted, ...) {
#'   f_score(observed, predicted, beta = 2, ...)
#' }
#' 
#' MLMetric(f2_score) <- list(name = "f2_score",
#'                            label = "F Score (beta = 2)",
#'                            maximize = TRUE)
#' 
"MLMetric<-" <- function(object, value) {
  do.call(MLMetric, c(object, value))
}
