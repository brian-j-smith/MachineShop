CForestModel <- function(control = NULL) {
  MLModel(
    name = "CForestModel",
    packages = "party",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      party::cforest(formula, data = data, ...) %>%
        asMLModelFit("CForestFit", CForestModel(...))
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


CoxModel <- function(ties = NULL, control = NULL) {
  MLModel(
    name = "CoxModel",
    packages = "survival",
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, ...) {
      survival::coxph(formula, data = data, x = TRUE, ...) %>%
        asMLModelFit("CoxFit", CoxModel(...))
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


CoxStepAICModel <- function(ties = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = NULL, steps = NULL)
  {
  MLModel(
    name = "CoxStepAICModel",
    packages = c("MASS", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, direction = c("both", "backward", "forward"),
                   scope = list(), k = 2, trace = 0, steps = 1000, ...) {
      fitStepAIC(function(formula, data) {
        survival::coxph(formula, data = data, x = TRUE, ...)
      }, data, formula, match.arg(direction), scope, k, trace, steps) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    },
    predict = CoxModel()@predict
  )
}


GBMModel <- function(distribution = NULL, n.trees = NULL,
                     interaction.depth = NULL, n.minobsinnode = NULL,
                     shrinkage = NULL, bag.fraction = NULL) {
  MLModel(
    name = "GBMModel",
    packages = "gbm",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      gbm::gbm(formula, data = data, ...) %>%
        asMLModelFit("GBMFit", GBMModel(...))
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


GLMModel <- function(family = NULL, control = NULL) {
  MLModel(
    name = "GLMModel",
    packages = "stats",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      stats::glm(formula, data = data, ...) %>%
        asMLModelFit("GLMFit", GLMModel(...))
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


GLMStepAICModel <- function(family = NULL, control = NULL, direction = NULL,
                            scope = NULL, k = NULL, trace = NULL, steps = NULL)
  {
  MLModel(
    name = "GLMStepAICModel",
    packages = c("MASS", "stats"),
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, direction = c("both", "backward", "forward"),
                   scope = list(), k = 2, trace = 0, steps = 1000, ...) {
      fitStepAIC(function(formula, data) {
        stats::glm(formula, data = data, ...)
      }, data, formula, match.arg(direction), scope, k, trace, steps) %>%
        asMLModelFit("GLMFit", GLMModel(...))
    },
    predict = GLMModel()@predict
  )
}


GLMNetModel <- function(family = NULL, alpha = NULL, lambda = NULL,
                        standardize = NULL, thresh = NULL, maxit = NULL,
                        type.gaussian = NULL, type.logistic = NULL,
                        type.multinomial = NULL) {
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
      asMLModelFit(mfit, "GLMNetFit", GLMNetModel(...))
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


NNetModel <- function(size, linout = NULL, entropy = NULL, softmax = NULL,
                      censored = NULL, skip = NULL, rang = NULL, decay = NULL,
                      maxit = NULL, Hess = NULL, trace = NULL, MaxNWts = NULL,
                      abstol = NULL, reltol = NULL) {
  MLModel(
    name = "NNetModel",
    packages = "nnet",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      mfit <- nnet::nnet(formula, data = data, trace = FALSE, ...)
      mfit$y <- response(formula, data)
      asMLModelFit(mfit, "NNetFit", NNetModel(...))
    },
    predict = function(object, data, type = "response", cutoff = 0.5, ...) {
      obs <- object$y
      object <- asParentFit(object)
      pred <- predict(object, newdata = data, type = "raw")
      if(type == "response") pred <- convert(obs, pred, cutoff = cutoff)
      pred
    }
  )
}


RandomForestModel <- function(ntree = NULL, mtry = NULL, replace = NULL,
                              nodesize = NULL, maxnodes = NULL) {
  MLModel(
    name = "RandomForestModel",
    packages = "randomForest",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      randomForest::randomForest(formula, data = data, ...) %>%
        asMLModelFit("RandomForestFit", RandomForestModel(...))
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


SurvRegModel <- function(dist = NULL, scale = NULL, parms = NULL,
                         control = NULL) {
  MLModel(
    name = "SurvRegModel",
    packages = c("rms", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, ...) {
      rms::psm(formula, data = data, ...) %>%
        asMLModelFit("SurvRegFit", SurvRegModel(...))
    },
    predict = function(object, data, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      if(length(times)) {
        pred <- rms::survest(object, newdata = data, times = times,
                             conf.int = FALSE)
        if(inherits(pred, "survest.psm")) pred <- as.matrix(pred$surv)
      } else {
        pred <- predict(object, newdata = data, type = "risk")
      }
      if(type == "response") {
        pred <- convert(response(object), pred, cutoff = cutoff)
      }
      pred
      
    }
  )
}


SurvRegStepAICModel <- function(dist = NULL, scale = NULL, parms = NULL,
                                control = NULL, direction = NULL, scope = NULL,
                                k = NULL, trace = NULL, steps = NULL) {
  MLModel(
    name = "SurvRegStepAICModel",
    packages = c("MASS", "rms", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, direction = c("both", "backward", "forward"),
                   scope = list(), k = 2, trace = 0, steps = 1000, ...) {
      fitStepAIC(function(formula, data) {
        rms::psm(formula, data = data, ...)
      }, data, formula, match.arg(direction), scope, k, trace, steps) %>%
        asMLModelFit("SurvRegFit", SurvRegModel(...))
    },
    predict = SurvRegModel()@predict
  )
}
