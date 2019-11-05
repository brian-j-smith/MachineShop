VarImp <- function(object, ...) {
  UseMethod("VarImp")
}


VarImp.default <- function(object, scale = TRUE, ...) {
  stopifnot(nrow(object) == 0 || is.character(rownames(object)))
  
  idx <- order(rowSums(object), decreasing = TRUE)
  idx <- idx * (rownames(object)[idx] != "(Intercept)")
  object <- object[idx, , drop = FALSE]
  if (scale) {
    object_shift <- min(object)
    object_scale <- diff(range(object)) / 100
    object <- (object - object_shift) / object_scale
  } else {
    object_shift = 0
    object_scale = 1
  }
  
  new("VarImp", object, shift = object_shift, scale = object_scale)
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
  model <- as.MLModel(object)
  requireModelNamespaces(model@packages)
  vi <- model@varimp(unMLModelFit(object), ...)
  if (is.null(vi)) vi <- varimp_undef(object)
  VarImp(vi, scale = scale)
}


varimp_pval <- function(object, ...) {
  UseMethod("varimp_pval")
}


varimp_pval.default <- function(object, ...) {
  varimp_pval(coef(object), diag(vcov(object)))
}


varimp_pval.glm <- function(object, ...) {
  anova <- drop1(object, test = "Chisq")
  -log(anova[-1, "Pr(>Chi)", drop = FALSE])
}


varimp_pval.lm <- function(object, ...) {
  anova <- drop1(object, test = "F")
  -log(anova[-1, "Pr(>F)", drop = FALSE])
}


varimp_pval.mlm <- function(object, ...) {
  varimp_pval.default(object)
}


varimp_pval.numeric <- function(object, var, ...) {
  -pchisq(object^2 / var, 1, lower.tail = FALSE, log.p = TRUE)
}


varimp_undef <- function(object) {
  warn("variable importance not defined for ", class(object)[1])
  varnames <- labels(terms(as.MLModel(object)@x))
  structure(rep(NA_real_, length(varnames)), names = varnames)
}
