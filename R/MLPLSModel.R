PLSModel <- function(ncomp = 1, scale = NULL) {
  MLModel(
    name = "PLSModel",
    packages = "pls",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      environment(formula) <- environment()
      y <- response(formula, data)
      if(is.factor(y)) {
        varname <- all.vars(formula)[1]
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
      predict(asParentFit(object), newdata = newdata, ncomp = object$ncomp,
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
      if(ncol(vi) <= 2) vi <- vi[,1]
      vi
    }
  )
}
