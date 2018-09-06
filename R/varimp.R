#' Variable Importance
#' 
#' Calculate measures of the relative importance of predictors in a model.
#' 
#' @param object MLModelFit object from a model fit.
#' @param scale logical indicating whether importance measures should be scaled
#' to range from 0 to 100.
#' @param ... arguments passed to model-specific variable importance functions.
#' 
#' @seealso \code{\link{fit}}, \code{\link[MLModels:plot.VarImp]{plot}}
#'
varimp <- function(object, scale = TRUE, ...) {
  new("VarImp", as(varimp_sub(object, ...), "VarImp"), scale = scale)
}


varimp_sub <- function(object, ...) {
  UseMethod("varimp_sub", object)
}


varimp_sub.MLModelFit <- function(object, ...) {
  requireModelNamespaces(field(object, ".packages"))
  varimp <- field(object, ".varimp")
  varimp(object, ...)
}
