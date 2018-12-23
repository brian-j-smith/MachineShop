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
#' Further model details can be found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[pls]{mvr}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = PLSModel())
#'
PLSModel <- function(ncomp = 1, scale = FALSE) {
  
  MLModel(
    name = "PLSModel",
    label = "Partial Least Squares",
    packages = "pls",
    types = c("factor", "numeric"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      assert_equal_weights(weights)
      y <- response(formula, data)
      if (is.factor(y)) {
        varname <- response(terms(formula))
        mm <- model.matrix(~ y - 1)
        colnames(mm) <- levels(y)
        data[[varname]] <- mm
        formula[[2]] <- as.symbol(varname)
      }
      pls::plsr(formula, data = data, ...)
    },
    predict = function(object, newdata, ...) {
      predict(object, newdata = newdata, ncomp = object$ncomp,
              type = "response")
    },
    varimp = function(object, ...) {
      beta <- coef(object, comps = 1:object$ncomp)
      perf <- quote(MSEP.mvr(x)$val[1, , , drop = FALSE]) %>%
        eval(list(x = object), asNamespace("pls"))
      vi <- sapply(1:dim(beta)[2], function(i) {
        as.matrix(abs(beta[, i, ])) %*% prop.table(-diff(perf[, i, ]))
      })
      dimnames(vi) <- dimnames(beta)[1:2]
      if (ncol(vi) <= 2) vi <- vi[, 1]
      vi
    }
  )
  
}
