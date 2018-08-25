setClass("CForestFit", contains = c("MLModelFit", "RandomForest"))

CForestModel <- function(...) {
  MLModel(
    name = "CForestModel",
    params = list(...),
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
        convert(object@responses@variables[[1]], pred, cutoff = cutoff)
      } else {
        pred
      }
    }
  )
}


CoxModel <- function(...) {
  MLModel(
    name = "CoxModel",
    params = list(...),
    fit = function(formula, data, ...) {
      coxph(formula, data = data, x = TRUE, ...) %>%
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
      if(type == "response") convert(object$y, pred, cutoff = cutoff) else pred
    }
  )
}


CoxStepAICModel <- function(...) {
  MLModel(
    name = "CoxStepAICModel",
    params = list(...),
    fit = function(formula, data, scope = list(),
                   direction = c("both", "backward", "forward"), trace = 0,
                   k = 2, ...) {
      if(is.null(scope$lower)) scope$lower <- ~ 1
      if(is.null(scope$upper)) scope$upper <- formula[-2]
      direction <- match.arg(direction)
      rhs <- if(direction == "backward") scope$upper else scope$lower
      formula <- update(formula, rhs)
      environment(formula) <- environment()
      fit0 <- coxph(formula, data = data, x = TRUE, ...)
      MASS::stepAIC(fit0, scope = scope, direction = direction, trace = trace,
                    k = k) %>%
        asMLModelFit("CoxFit", CoxModel())
    },
    predict = CoxModel()@predict
  )
}


GBMModel <- function(...) {
  MLModel(
    name = "GBMModel",
    params = list(...),
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
        convert(response.gbm(object), pred, cutoff = cutoff)
      } else {
        pred
      }
    }
  )
}


GLMNetModel <- function(...) {
  MLModel(
    name = "GLMNetModel",
    params = list(...),
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
      if(type == "response") convert(y, pred, cutoff = cutoff) else pred
    }
  )
}
