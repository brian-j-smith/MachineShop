VarImp <- function(object, ...) {
  UseMethod("VarImp")
}


VarImp.default <- function(object, scale = TRUE, ...) {
  stopifnot(nrow(object) == 0 || is.character(rownames(object)))

  idx <- order(rowSums(object), decreasing = TRUE)
  idx <- idx * (rownames(object)[idx] != "(Intercept)")
  object <- object[idx, , drop = FALSE]
  if (scale) {
    scale_center <- min(object)
    scale_scale <- diff(range(object)) / 100
    object <- (object - scale_center) / scale_scale
  } else {
    scale_center = 0
    scale_scale = 1
  }
  
  new("VarImp", object, center = scale_center, scale = scale_scale)
}


VarImp.matrix <- function(object, ...) {
  VarImp(as.data.frame(object), ...)
}


VarImp.numeric <- function(object, ...) {
  VarImp(cbind(Overall = object), ...)
}


#' Variable Importance
#' 
#' Calculate measures of the relative importance of predictors in a model.
#' 
#' @param object model \link{fit} result.
#' @param scale logical indicating whether importance measures should be scaled
#'   to range from 0 to 100.
#' @param ... arguments passed to model-specific variable importance functions.
#' 
#' @return \code{VarImp} class object.
#' 
#' @seealso \code{\link{plot}}
#'
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' gbm_fit <- fit(Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
#'                data = Melanoma, model = GBMModel)
#' (vi <- varimp(gbm_fit))
#' plot(vi)
#'
varimp <- function(object, scale = TRUE, ...) {
  stopifnot(is(object, "MLModelFit"))
  requireModelNamespaces(modelbits(object, "packages"))
  vi <- modelbits(object, "varimp")(unMLModelFit(object), ...)
  if (is.null(vi)) vi <- varimp_undef(object)
  VarImp(vi, scale = scale)
}


varimp_wald <- function(object, ...) {
  UseMethod("varimp_wald")
}


varimp_wald.default <- function(object, ...) {
  varimp_wald(coef(object), diag(vcov(object)))
}


varimp_wald.numeric <- function(object, var, ...) {
  pchisq(object^2 / var, 1)
}


varimp_undef <- function(object) {
  warn("variable importance not defined for ", class(object)[1])
  varnames <- labels(terms(modelbits(object, "x")))
  structure(rep(NA_real_, length(varnames)), names = varnames)
}
