setClass("CForestFit", contains = "RandomForest")

setClass("CForestModel", contains = "AbstractModel")

CForestModel <- function(...) new("CForestModel", ...)

setMethod("initialize", "CForestModel",
  function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@fit <- function(formula, data, ...) {
      mfit <- party::cforest(formula, data = data, ...)
      as(mfit, "CForestFit")
    }
    .Object@predict <- function(object, data, type = "response", cutoff = 0.5,
                                times = numeric(), ...) {
      object <- as(object, extends(class(object))[2])
      pred <- if(object@responses@is_censored) {
        if(length(times)) {
          predict(object, newdata = data, type = "prob") %>%
            sapply(function(fit) probs.survfit(fit, times)) %>%
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
    .Object
  }
)


setClass("CoxModel", contains = "AbstractModel")

CoxModel <- function(...) new("CoxModel", ...)

setMethod("initialize", "CoxModel",
  function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@fit <- function(formula, data, ...) {
      mfit <- coxph(formula, data = data, x = TRUE, ...)
      structure(mfit, class = c("CoxFit", class(mfit)))
    }
    .Object@predict <- function(object, data, type = "response", cutoff = 0.5,
                                times = numeric(), ...) {
      class(object) <- class(object)[-1]
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
    .Object
  }
)


setClass("CoxStepAICModel", contains = "AbstractModel")

CoxStepAICModel <- function(...) new("CoxStepAICModel", ...)

setMethod("initialize", "CoxStepAICModel",
  function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@fit <- function(formula, data, scope = list(),
                            direction = c("both", "backward", "forward"),
                            trace = 0, k = 2, ...) {
      if(is.null(scope$lower)) scope$lower <- ~ 1
      if(is.null(scope$upper)) scope$upper <- formula[-2]
      direction <- match.arg(direction)
      rhs <- if(direction == "backward") scope$upper else scope$lower
      formula <- update(formula, rhs)
      environment(formula) <- environment()
      fit0 <- coxph(formula, data = data, x = TRUE, ...)
      mfit <- MASS::stepAIC(fit0, scope = scope, direction = direction,
                            trace = trace, k = k)
      structure(mfit, class = c("CoxFit", class(mfit)))
    }
    .Object@predict <- function(object, data, type = "response", cutoff = 0.5,
                                times = numeric(), ...) {
      CoxModel()@predict(object, data, type = type, times = times,
                         cutoff = cutoff)
    }
    .Object
  }
)


setClass("GBMModel", contains = "AbstractModel")

GBMModel <- function(...) new("GBMModel", ...)

setMethod("initialize", "GBMModel",
  function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@fit <- function(formula, data, ...) {
      mfit <- gbm::gbm(formula, data = data, ...)
      structure(mfit, class = c("GBMFit", class(mfit)))
    }
    .Object@predict <- function(object, data, type = "response", cutoff = 0.5,
                                times = numeric(), ...) {
      class(object) <- class(object)[-1]
      pred <- if(object$distribution$name == "coxph") {
        if(length(times)) {
          lp <- predict(object, n.trees = object$n.trees, type = "link")
          newlp <- predict(object, newdata = data, n.trees = object$n.trees,
                           type = "link")
          cumhaz <- basehaz(response.gbm(object), exp(lp), times)
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
    .Object
  }
)


setClass("GLMNetModel", contains = "AbstractModel")

GLMNetModel <- function(...) new("GLMNetModel", ...)

setMethod("initialize", "GLMNetModel",
  function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@fit <- function(formula, data, ...) {
      mf <- model.frame(formula, data, na.action = NULL)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      mfit <- glmnet::glmnet(x, y, nlambda = 1, ...)
      structure(c(mfit, list(mf = mf)), class = c("GLMNetFit", class(mfit)))
    }
    .Object@predict <- function(object, data, type = "response", cutoff = 0.5,
                                times = numeric(), ...) {
      class(object) <- class(object)[-1]
      obj_terms <- terms(object$mf)
      newmf <- model.frame(obj_terms, data, na.action = NULL)
      newx <- model.matrix(obj_terms, newmf)[, -1, drop = FALSE]
      y <- model.response(object$mf)
      pred <- if(is.Surv(y)) {
        if(length(times)) {
          x <- model.matrix(obj_terms, object$mf)[, -1, drop = FALSE]
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
    .Object
  }
)
