AbstractModel <- R6Class(
  "AbstractModel",
  public = list(
    params = NULL,
    initialize = function(...) {
      self$params <- list(...)
      self
    },
    fit = function(formula, data) {
      NULL
    },
    predict = function(object, data, times = NULL) {
      NULL
    }
  )
)


CForestModel <- R6Class(
  "CForestModel",
  inherit = AbstractModel,
  public = list(
    fit = function(formula, data) {
      mfit <- do.call(function(...) party::cforest(formula, data = data, ...),
                      self$params)
      as(mfit, "CForestFit")
    },
    predict = function(object, data, times = NULL) {
      object <- as(object, extends(class(object))[2])
      if(object@responses@is_censored) {
        if(is.numeric(times)) {
          predict(object, newdata = data, type = "prob") %>%
            sapply(function(fit) probs.survfit(fit, times)) %>%
            t
        } else {
          log(2) / predict(object, newdata = data, type = "response")
        }
      } else {
        predict(object, newdata = data, type = "prob") %>%
          unlist %>%
          matrix(nrow = nrow(data), byrow = TRUE)
      }
    }
  )
)


CoxModel <- R6Class(
  "CoxModel",
  inherit = AbstractModel,
  public = list(
    fit = function(formula, data) {
      mfit <- do.call(function(...) coxph(formula, data = data, x = TRUE, ...),
                      self$params)
      structure(mfit, class = c("CoxFit", class(mfit)))
    },
    predict = function(object, data, times = NULL) {
      class(object) <- class(object)[-1]
      if(is.numeric(times)) {
        timevar <- all.vars(object$formula)[1]
        sapply(times, function(time) {
          data[[timevar]] <- time
          exp(-predict(object, newdata = data, type = "expected"))
        })
      } else {
        predict(object, newdata = data, type = "risk")
      }
    }
  )
)


CoxStepAICModel <- R6Class(
  "CoxStepAICModel",
  inherit = AbstractModel,
  public = list(
    scope = NULL,
    direction = NULL,
    k = NULL,
    trace = NULL,
    initialize = function(scope, direction = c("both", "backward", "forward"),
                          trace = 0, k = 2, ...) {
      super$initialize(...)
      if(!missing(scope)) self$scope <- scope
      self$direction <- match.arg(direction)
      self$trace <- trace
      self$k <- k
    },
    fit = function(formula, data) {
      scope <- self$scope
      if(is.null(scope$lower)) scope$lower <- ~ 1
      if(is.null(scope$upper)) scope$upper <- formula[-2]
      rhs <- if(self$direction == "backward") scope$upper else scope$lower
      formula <- update(formula, rhs)
      environment(formula) <- environment()
      fit0 <- do.call(function(...) coxph(formula, data = data, x = TRUE, ...),
                      self$params)
      mfit <- MASS::stepAIC(fit0, scope = scope, direction = self$direction,
                            trace = self$trace, k = self$k)
      structure(mfit, class = c("CoxFit", class(mfit)))
    },
    predict = function(object, data, times = NULL) {
      CoxModel$new()$predict(object, data, times)
    }
  )
)


GBMModel <- R6Class(
  "GBMModel",
  inherit = AbstractModel,
  public = list(
    fit = function(formula, data) {
      mfit <- do.call(function(...) gbm::gbm(formula, data = data, ...),
                      self$params)
      structure(mfit, class = c("GBMFit", class(mfit)))
    },
    predict = function(object, data, times = NULL) {
      class(object) <- class(object)[-1]
      if(object$distribution == "coxph") {
        if(is.numeric(times)) {
          lp <- predict(object, n.trees = object$n.trees, type = "link")
          newlp <- predict(object, newdata = data, n.trees = object$n.trees,
                           type = "link")
          idx <- order(object$data$i.timeorder)
          cumhaz <- basehaz(Surv(object$data$y[idx], object$data$Misc[idx]),
                            exp(lp), times)
          exp(exp(newlp - mean(lp)) %o% -cumhaz)
        } else {
          exp(predict(object, newdata = data, n.trees = object$n.trees,
                      type = "link"))
        }
      } else {
        predict(object, newdata = data, n.trees = object$n.trees,
                type = "response") %>% drop
      }
    }
  )
)


GLMNetModel <- R6Class(
  "GLMNetModel",
  inherit = AbstractModel,
  public = list(
    fit = function(formula, data) {
      mf <- model.frame(formula, data, na.action = NULL)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      mfit <- do.call(function(...) glmnet::glmnet(x, y, nlambda = 1, ...),
                      self$params)
      structure(c(mfit, list(mf = mf)), class = c("GLMNetFit", class(mfit)))
    },
    predict = function(object, data, times = NULL) {
      class(object) <- class(object)[-1]
      obj_terms <- terms(object$mf)
      newmf <- model.frame(obj_terms, data, na.action = NULL)
      newx <- model.matrix(obj_terms, newmf)[, -1, drop = FALSE]
      y <- model.response(object$mf)
      if(is.Surv(y)) {
        if(is.numeric(times)) {
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
    }
  )
)
