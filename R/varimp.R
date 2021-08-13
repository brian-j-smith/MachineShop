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
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Survival response example
#' library(survival)
#'
#' gbm_fit <- fit(Surv(time, status) ~ ., data = veteran, model = GBMModel)
#' (vi <- varimp(gbm_fit))
#' plot(vi)
#' }
#'
varimp <- function(object, scale = TRUE, ...) {
  stopifnot(is(object, "MLModelFit"))
  model <- as.MLModel(object)
  require_namespaces(model@packages)
  vi <- model@varimp(unMLModelFit(object), ...)
  if (is.null(vi)) {
    throw(Warning("variable importance not defined for ", class(object)[1]))
    vi <- varimp_undef(model@x)
  }
  VarImp(vi, scale = scale)
}


varimp_pval <- function(object, ...) {
  UseMethod("varimp_pval")
}


varimp_pval.default <- function(object, ...) {
  varimp_pval(coef(object), diag(vcov(object)), ...)
}


varimp_pval.glm <- function(object, base = exp(1), ...) {
  anova <- drop1(object, test = "Chisq")
  -log(anova[-1, "Pr(>Chi)", drop = FALSE], base = base)
}


varimp_pval.lm <- function(object, base = exp(1), ...) {
  anova <- drop1(object, test = "F")
  -log(anova[-1, "Pr(>F)", drop = FALSE], base = base)
}


varimp_pval.mlm <- function(object, ...) {
  check <- check_equal_weights(object$weights)
  if (is(check, "warning")) {
    throw(LocalWarning("variable importance not defined for ", class(object)[1],
                       " with case weights"))
    varimp_undef(object)
  } else {
    object$weights <- NULL
    varimp_pval.default(object, ...)
  }
}


varimp_pval.multinom <- function(object, ...) {
  varimp_pval(t(coef(object)), diag(vcov(object)), ...)
}


varimp_pval.numeric <- function(object, var, base = exp(1), ...) {
  -log(pchisq(object^2 / var, 1, lower.tail = FALSE), base = base)
}


varimp_undef <- function(object) {
  var_names <- labels(terms(object))
  structure(rep(NA_real_, length(var_names)), names = var_names)
}
