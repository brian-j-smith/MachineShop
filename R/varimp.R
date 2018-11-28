#' Variable Importance
#' 
#' Calculate measures of the relative importance of predictors in a model.
#' 
#' @param object \code{MLModelFit} object from a model fit.
#' @param scale logical indicating whether importance measures should be scaled
#' to range from 0 to 100.
#' @param ... arguments passed to model-specific variable importance functions.
#' 
#' @return \code{VarImp} class object.
#' 
#' @seealso \code{\link{fit}}, \code{\link{plot}}
#'
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' gbmfit <- fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'               data = Melanoma, model = GBMModel)
#' (vi <- varimp(gbmfit))
#' plot(vi)
#'
varimp <- function(object, scale = TRUE, ...) {
  stopifnot(is(object, "MLModelFit"))
  requireModelNamespaces(fitbit(object, "packages"))
  vi <- fitbit(object, "varimp")(unMLModelFit(object), ...)
  if (is.null(vi)) vi <- varimp_undef(object)
  VarImp(as(vi, "VarImp"), scale = scale)
}


varimp_pchisq <- function(object) {
  pchisq(coef(object)^2 / diag(vcov(object)), 1)
}


varimp_undef <- function(object) {
  warn("variable importance not defined for ", class(object)[1])
  varnames <- labels(terms(fitbit(object, "x")))
  structure(rep(NA_integer_, length(varnames)), names = varnames)
}
