#' Partial Least Squares Model
#'
#' Function to perform partial least squares regression.
#' 
#' @param ncomp number of components to include in the model.
#' @param scale logical indicating whether to scale the predictors by the
#' sample standard deviation.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[pls]{mvr}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#'
PLSModel <- function(ncomp = 1, scale = NULL) {
  MLModel(
    name = "PLSModel",
    packages = "pls",
    types = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = NULL, ...) {
      if(!is.null(weights)) warning("weights are unsupported and will be ignored")
      environment(formula) <- environment()
      y <- response(formula, data)
      if(is.factor(y)) {
        varname <- response(terms(formula))
        mm <- model.matrix(~ y - 1)
        colnames(mm) <- levels(y)
        data[[varname]] <- I(mm)
        formula[[2]] <- as.symbol(varname)
      }
      mfit <- pls::plsr(formula, data = data, ...)
      mfit$y <- y
      asMLModelFit(mfit, "PLSFit", PLSModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(unMLModelFit(object), newdata = newdata, ncomp = object$ncomp,
              type = "response") %>% drop
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      beta <- coef(object, comps = 1:object$ncomp)
      perf <- quote(MSEP.mvr(x)$val[1, , , drop = FALSE]) %>%
        eval(list(x = object), asNamespace("pls"))
      vi <- sapply(1:dim(beta)[2], function(i) {
        as.matrix(abs(beta[, i, ])) %*% prop.table(-diff(perf[, i, ]))
      })
      dimnames(vi) <- dimnames(beta)[1:2]
      if(ncol(vi) <= 2) vi <- vi[, 1]
      vi
    }
  )
}
