GLMNetModel <- function(family = NULL, alpha = NULL, lambda = NULL,
                        standardize = NULL, thresh = NULL, maxit = NULL,
                        type.gaussian = NULL, type.logistic = NULL,
                        type.multinomial = NULL) {
  MLModel(
    name = "GLMNetModel",
    packages = "glmnet",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      mf <- model.frame(formula, data, na.action = NULL)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      mfit <- glmnet::glmnet(x, y, weights = weights, nlambda = 1, ...)
      mfit$mf <- mf
      asMLModelFit(mfit, "GLMNetFit", GLMNetModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      mf <- object$mf
      object <- asParentFit(object)
      obj_terms <- terms(mf)
      newmf <- model.frame(obj_terms, newdata, na.action = NULL)
      newx <- model.matrix(obj_terms, newmf)[, -1, drop = FALSE]
      y <- model.response(mf)
      if(is.Surv(y)) {
        if(length(times)) {
          x <- model.matrix(obj_terms, mf)[, -1, drop = FALSE]
          lp <- predict(object, newx = x, type = "link") %>% drop
          newlp <- predict(object, newx = newx, type = "link") %>% drop
          cumhaz <- basehaz(y, exp(lp), times)
          exp(exp(newlp - mean(lp)) %o% -cumhaz)
        } else {
          exp(predict(object, newx = newx, type = "link")) %>% drop
        }
      } else {
        predict(object, newx = newx, type = "response") %>% drop
      }
    },
    response = function(object, ...) {
      response(object$mf)
    },
    varimp = function(object, ...) {
      convert <- function(x) drop(as.matrix(x))
      beta <- object$beta
      if(is.list(beta)) as.data.frame(lapply(beta, convert)) else convert(beta)
    }
  )
}
