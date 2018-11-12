#' Variable Importance
#' 
#' Calculate measures of the relative importance of predictors in a model.
#' 
#' @param object MLModelFit object from a model fit.
#' @param scale logical indicating whether importance measures should be scaled
#' to range from 0 to 100.
#' @param ... arguments passed to model-specific variable importance functions.
#' 
#' @return VarImp class object.
#' 
#' @seealso \code{\link{fit}}, \code{\link{plot}}
#'
#' @examples
#' ## Survival response example
#' library(survival)
#' 
#' gbmfit <- fit(Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
#'                 pat.karno + meal.cal + wt.loss, data = lung,
#'                 model = GBMModel)
#' (vi <- varimp(gbmfit))
#' plot(vi)
#'
varimp <- function(object, scale = TRUE, ...) {
  requireModelNamespaces(field(object, ".packages"))
  varimp <- field(object, ".varimp")
  VarImp(as(varimp(object, ...), "VarImp"), scale = scale)
}
