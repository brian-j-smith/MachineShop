C50Model <- function(trials = NULL, rules = NULL, control = NULL, costs = NULL)
  {
  MLModel(
    name = "C50Model",
    packages = "C50",
    responses = "factor",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      C50::C5.0(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("C50Fit", C50Model(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, type = "prob")
      if(type == "response") {
        pred <- convert(factor(object$levels), pred, cutoff = cutoff)
      }
      pred
    }
  )
}


CForestModel <- function(control = NULL) {
  MLModel(
    name = "CForestModel",
    packages = "party",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      party::cforest(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("CForestFit", CForestModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      pred <- if(object@responses@is_censored) {
        if(length(times)) {
          predict(object, newdata = newdata, type = "prob") %>%
            lapply(function(fit) predict(fit, times)) %>%
            (function(args) do.call(rbind, args))
        } else if(type == "response") {
          predict(object, newdata = newdata, type = "response")
        } else {
          log(2) / predict(object, newdata = newdata, type = "response")
        }
      } else {
        predict(object, newdata = newdata, type = "prob") %>%
          unlist %>%
          matrix(nrow = nrow(newdata), byrow = TRUE) %>%
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
    packages = c("rms", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      rms::cph(formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        asMLModelFit("CoxFit", CoxModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      pred <- if(length(times)) {
        rms::survest(object, newdata = newdata, times = times,
                     conf.int = FALSE, se.fit = FALSE)$surv %>% as.matrix
      } else {
        exp(predict(object, newdata = newdata, type = "lp"))
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
    packages = c("MASS", "rms", "survival"),
    responses = "Surv",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 0, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      args <- argsStepAIC(formula, direction, scope)
      rms::cph(args$formula, data = data, weights = weights, singular.ok = TRUE,
               surv = TRUE, y = TRUE, ...) %>%
        MASS::stepAIC(direction = direction, scope = args$scope, k = k,
                      trace = trace, steps = steps) %>%
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
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      gbm::gbm(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("GBMFit", GBMModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      pred <- if(object$distribution$name == "coxph") {
        if(length(times)) {
          lp <- predict(object, n.trees = object$n.trees, type = "link")
          newlp <- predict(object, newdata = newdata, n.trees = object$n.trees,
                           type = "link")
          cumhaz <- basehaz(response(object), exp(lp), times)
          exp(exp(newlp - mean(lp)) %o% -cumhaz)
        } else {
          exp(predict(object, newdata = newdata, n.trees = object$n.trees,
                      type = "link"))
        }
      } else {
        predict(object, newdata = newdata, n.trees = object$n.trees,
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
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      stats::glm(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("GLMFit", GLMModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, type = "response")
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
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 0, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      args <- argsStepAIC(formula, direction, scope)
      stats::glm(args$formula, data = data, weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = args$scope, k = k,
                      trace = trace, steps = steps) %>%
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
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      mf <- model.frame(formula, data, na.action = NULL)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      mfit <- glmnet::glmnet(x, y, weights = weights, nlambda = 1, ...)
      mfit$mf <- mf
      asMLModelFit(mfit, "GLMNetFit", GLMNetModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      mf <- object$mf
      object <- asParentFit(object)
      obj_terms <- terms(mf)
      newmf <- model.frame(obj_terms, newdata, na.action = NULL)
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


KSVMModel <- function(scaled = NULL, type = NULL, kernel = NULL, kpar = NULL,
                      C = NULL, nu = NULL, epsilon = NULL, cache = NULL,
                      tol = NULL, shrinking = NULL) {
  MLModel(
    name = "KSVMModel",
    packages = "kernlab",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      environment(formula) <- environment()
      kernlab::ksvm(formula, data = data, prob.model = TRUE, ...) %>%
        asMLModelFit("KSVMFit", KSVMModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      obs <- response(object)
      pred <- kernlab::predict(object, newdata = newdata,
                               type = ifelse(is.factor(obs),
                                             "probabilities", "response"))
      if(type == "response") pred <- convert(obs, pred, cutoff = cutoff)
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
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      mfit <- nnet::nnet(formula, data = data, weights = weights, trace = FALSE,
                         ...)
      mfit$y <- response(formula, data)
      asMLModelFit(mfit, "NNetFit", NNetModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      obs <- object$y
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, type = "raw")
      if(type == "response") pred <- convert(obs, pred, cutoff = cutoff)
      pred
    }
  )
}


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
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      obs <- object$y
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, ncomp = object$ncomp,
                      type = "response") %>% drop
      if(type == "response") pred <- convert(obs, pred, cutoff = cutoff)
      pred
    }
  )
}


POLRModel <- function(method = NULL) {
  MLModel(
    name = "POLRModel",
    packages = "MASS",
    responses = "ordered",
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      MASS::polr(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("POLRModel", POLRModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      pred <- predict(object, newdata = newdata, type = "probs")
      if(type == "response") {
        pred <- convert(factor(object$lev), pred, cutoff = cutoff)
      }
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
      environment(formula) <- environment()
      randomForest::randomForest(formula, data = data, ...) %>%
        asMLModelFit("RandomForestFit", RandomForestModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      obs <- response(object)
      pred <- predict(object, newdata = newdata,
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
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      rms::psm(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("SurvRegFit", SurvRegModel(...))
    },
    predict = function(object, newdata, type = "response", cutoff = 0.5,
                       times = numeric(), ...) {
      object <- asParentFit(object)
      if(length(times)) {
        pred <- rms::survest(object, newdata = newdata, times = times,
                             conf.int = FALSE)
        if(inherits(pred, "survest.psm")) pred <- as.matrix(pred$surv)
      } else {
        pred <- exp(predict(object, newdata = newdata, type = "lp"))
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
    fit = function(formula, data, weights = rep(1, nrow(data)),
                   direction = c("both", "backward", "forward"), scope = list(),
                   k = 2, trace = 0, steps = 1000, ...) {
      environment(formula) <- environment()
      direction <- match.arg(direction)
      args <- argsStepAIC(formula, direction, scope)
      rms::psm(args$formula, data = data, weights = weights, ...) %>%
        MASS::stepAIC(direction = direction, scope = args$scope, k = k,
                      trace = trace, steps = steps) %>%
        asMLModelFit("SurvRegFit", SurvRegModel(...))
    },
    predict = SurvRegModel()@predict
  )
}
