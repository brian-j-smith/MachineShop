PLSModel <- function(ncomp, scale = NULL) {
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
        data[[varname]] <- I(model.matrix(~ y - 1))
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
    }
  )
}
