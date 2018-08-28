CForestModel <- function(control) {
  MLModel(
    name = "CForestModel",
    packages = "party",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      party::cforest(formula, data = data, ...) %>%
        asMLModelFit("CForestFit", CForestModel())
    },
    predict = function(object, data, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      pred <- if(object@responses@is_censored) {
        if(length(times)) {
          predict(object, newdata = data, type = "prob") %>%
            sapply(function(fit) predict(fit, times)) %>%
            t
        } else if(type == "response") {
          predict(object, newdata = data, type = "response")
        } else {
          log(2) / predict(object, newdata = data, type = "response")
        }
      } else {
        predict(object, newdata = data, type = "prob") %>%
          unlist %>%
          matrix(nrow = nrow(data), byrow = TRUE) %>%
          drop
      }
      if(type == "response") {
        pred <- convert(response(object), pred, cutoff = cutoff)
      }
      pred
    }
  )
}


CoxModel <- function(ties, control) {
  MLModel(
    name = "CoxModel",
    packages = "survival",
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, ...) {
      survival::coxph(formula, data = data, x = TRUE, ...) %>%
        asMLModelFit("CoxFit", CoxModel())
    },
    predict = function(object, data, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      pred <- if(length(times)) {
        timevar <- all.vars(object$formula)[1]
        sapply(times, function(time) {
          data[[timevar]] <- time
          exp(-predict(object, newdata = data, type = "expected"))
        })
      } else {
        predict(object, newdata = data, type = "risk")
      }
      if(type == "response") {
        pred <- convert(response(object), pred, cutoff = cutoff)
      }
      pred
    }
  )
}


CoxStepAICModel <- function(ties, control, direction, scope, k, trace, steps) {
  MLModel(
    name = "CoxStepAICModel",
    packages = c("MASS", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, direction = c("both", "backward", "forward"),
                   scope = list(), k = 2, trace = 0, steps = 1000, ...) {
      if(is.null(scope$lower)) scope$lower <- ~ 1
      if(is.null(scope$upper)) scope$upper <- formula[-2]
      direction <- match.arg(direction)
      rhs <- if(direction == "backward") scope$upper else scope$lower
      formula <- update(formula, rhs)
      environment(formula) <- environment()
      fit0 <- survival::coxph(formula, data = data, x = TRUE, ...)
      MASS::stepAIC(fit0, direction = direction, scope = scope, k = k,
                    trace = trace, steps = steps) %>%
        asMLModelFit("CoxFit", CoxModel())
    },
    predict = CoxModel()@predict
  )
}


GBMModel <- function(distribution, n.trees, interaction.depth, n.minobsinnode,
                     shrinkage, bag.fraction) {
  MLModel(
    name = "GBMModel",
    packages = "gbm",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      gbm::gbm(formula, data = data, ...) %>%
        asMLModelFit("GBMFit", GBMModel())
    },
    predict = function(object, data, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      pred <- if(object$distribution$name == "coxph") {
        if(length(times)) {
          lp <- predict(object, n.trees = object$n.trees, type = "link")
          newlp <- predict(object, newdata = data, n.trees = object$n.trees,
                           type = "link")
          cumhaz <- basehaz(response(object), exp(lp), times)
          exp(exp(newlp - mean(lp)) %o% -cumhaz)
        } else {
          exp(predict(object, newdata = data, n.trees = object$n.trees,
                      type = "link"))
        }
      } else {
        predict(object, newdata = data, n.trees = object$n.trees,
                type = "response") %>% drop
      }
      if(type == "response") {
        pred <- convert(response(object), pred, cutoff = cutoff)
      }
      pred
    }
  )
}


GLMModel <- function(family, control) {
  MLModel(
    name = "GLMModel",
    packages = "stats",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      stats::glm(formula, data = data, ...) %>%
        asMLModelFit("GLMFit", GLMModel())
    },
    predict = function(object, data, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      pred <- predict(object, newdata = data, type = "response")
      if(type == "response") {
        pred <- convert(response(object), pred, cutoff = cutoff)
      }
      pred
    }
  )
}


GLMStepAICModel <- function(family, control, direction, scope, k, trace, steps)
  {
  MLModel(
    name = "GLMStepAICModel",
    packages = c("MASS", "stats"),
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, direction = c("both", "backward", "forward"),
                   scope = list(), k = 2, trace = 0, steps = 1000, ...) {
      if(is.null(scope$lower)) scope$lower <- ~ 1
      if(is.null(scope$upper)) scope$upper <- formula[-2]
      direction <- match.arg(direction)
      rhs <- if(direction == "backward") scope$upper else scope$lower
      formula <- update(formula, rhs)
      environment(formula) <- environment()
      fit0 <- stats::glm(formula, data = data, ...)
      MASS::stepAIC(fit0, scope = scope, direction = direction, trace = trace,
                    steps = steps, k = k) %>%
        asMLModelFit("GLMFit", GLMModel())
    },
    predict = GLMModel()@predict
  )
}


GLMNetModel <- function(family, alpha, lambda, standardize, thresh, maxit,
                        type.gaussian, type.logistic, type.multinomial) {
  MLModel(
    name = "GLMNetModel",
    packages = "glmnet",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      mf <- model.frame(formula, data, na.action = NULL)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      mfit <- glmnet::glmnet(x, y, nlambda = 1, ...)
      mfit$mf <- mf
      asMLModelFit(mfit, "GLMNetFit", GLMNetModel())
    },
    predict = function(object, data, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      mf <- object$mf
      object <- asParentFit(object)
      obj_terms <- terms(mf)
      newmf <- model.frame(obj_terms, data, na.action = NULL)
      newx <- model.matrix(obj_terms, newmf)[, -1, drop = FALSE]
      y <- model.response(mf)
      pred <- if(is.Surv(y)) {
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
      if(type == "response") {
        pred <- convert(y, pred, cutoff = cutoff)
      }
      pred
    }
  )
}


RandomForestModel <- function(ntree, mtry, replace, nodesize, maxnodes) {
  MLModel(
    name = "RandomForestModel",
    packages = "randomForest",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      randomForest::randomForest(formula, data = data, ...) %>%
        asMLModelFit("RandomForestFit", RandomForestModel())
    },
    predict = function(object, data, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      obs <- response(object)
      pred <- predict(object, newdata = data,
                      type = ifelse(is.factor(obs), "prob", "response"))
      if(type == "response") {
        pred <- convert(obs, pred, cutoff = cutoff)
      }
      pred
    }
  )
}
