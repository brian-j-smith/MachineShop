#' Stacked Regression Model
#' 
#' Fit a stacked regression model from multiple base learners.
#' 
#' @param ... MLModel objects to serve as base learners.
#' @param control \code{\linkS4class{MLControl}} object, control function, or
#' character string naming a control function defining the resampling method to
#' be employed for the estimation of base learner weights.
#' @param weights optional fixed base learner weights.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#' \code{Surv}
#' }
#' }
#' 
#' @return StackedModel class object that inherits from MLModel.
#' 
#' @references
#' Breiman, L. (1996) \emph{Stacked Regression.} Machine Learning, 24, 49--64.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' model <- StackedModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' modelfit <- fit(medv ~ ., data = Boston, model = model)
#' predict(modelfit, newdata = Boston)
#' 
StackedModel <- function(..., control = CVControl, weights = NULL) {
  
  base_learners <- lapply(list(...), getMLObject, class = "MLModel")
  
  control <- getMLObject(control, "MLControl")
  control@summary <- function(observed, predicted, ...) NA
  
  if (!is.null(weights)) stopifnot(length(weights) == length(base_learners))
  
  new("StackedModel",
    name = "StackedModel",
    types = c("factor", "numeric", "ordered", "Surv"),
    params = as.list(environment()),
    predict = function(object, newdata, ...) {
      predicted <- 0
      for (i in seq(object$stack)) {
        predicted <- predicted +
          object$weights[i] * predict(object$stack[[i]], newdata = newdata,
                                      times = object$times, type = "prob")
      }
      predicted
    },
    varimp = function(object, ...) {
      warning("variable importance values undefined for StackedModel")
      varnames <- all.vars(object$formula[[3]])
      structure(rep(NA_integer_, length(varnames)), names = varnames)
    }
  )
  
}


setClass("StackedModel", contains = "MLModel")


setMethod(".fit", c("StackedModel", "ModelFrame"),
  function(model, x, ...) {
    modelfit <- .fit.StackedModel(model, x)
    modelfit$formula <- formula(terms(x))
    modelfit
  }
)


setMethod(".fit", c("StackedModel", "recipe"),
  function(model, x, ...) {
    modelfit <- .fit.StackedModel(model, x)
    modelfit$formula <- formula(prep(x))
    modelfit
  }
)


.fit.StackedModel <- function(model, x) {
  base_learners <- model@params$base_learners
  weights <- model@params$weights
  control <-  model@params$control
  times <- control@surv_times
  if (is.null(weights)) {
    num_learners <- length(base_learners)
    learner_responses <- list()
    complete_responses <- TRUE
    for (i in 1:num_learners) {
      learner_responses[[i]] <-
        resample(x, model = base_learners[[i]], control = control)@response
      complete_responses <-
        complete_responses & complete.cases(learner_responses[[i]])
    }
    if (control@na.rm) {
      learner_responses <- lapply(learner_responses, function(response) {
        response[complete_responses, , drop = FALSE]
      })
    }
    weights <- solnp(rep(1 / num_learners, num_learners),
                     function(x) stacked_eval(x, learner_responses, times),
                     eqfun = function(x) sum(x), eqB = 1,
                     LB = rep(0, num_learners),
                     control = list(trace = FALSE))$pars
  }
  list(stack = lapply(base_learners, function(learner) fit(x, learner)),
       weights = weights,
       times = times) %>%
    asMLModelFit("StackedModelFit", model, response(x))
}


stacked_eval <- function(weights, responses, times) {
  predicted <- 0
  for (i in seq(responses)) {
    predicted <- predicted + weights[i] * responses[[i]]$Predicted
  }
  df <- responses[[1]]["Observed"]
  df$Predicted <- predicted
  by(df, responses[[1]]$Resample, function(x) {
    stacked_loss(x$Observed, x$Predicted, times = times)
  }) %>% mean(na.rm = TRUE)
}


setGeneric("stacked_loss",
           function(observed, predicted, ...) standardGeneric("stacked_loss"))


setMethod("stacked_loss", c("factor", "matrix"),
  function(observed, predicted, ...) {
    observed <- model.matrix(~ observed - 1)
    if (ncol(predicted) == 1) predicted <- cbind(predicted, 1 - predicted)
    stacked_loss(observed, predicted)
  }
)


setMethod("stacked_loss", c("factor", "numeric"),
  function(observed, predicted, ...) {
    stacked_loss(observed, as.matrix(predicted))
  }
)


setMethod("stacked_loss", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    stopifnot(ncol(observed) == ncol(predicted))
    sum((observed - predicted)^2) / nrow(observed)
  }
)


setMethod("stacked_loss", c("numeric", "ANY"),
  function(observed, predicted, ...) {
    stacked_loss(as.matrix(observed), as.matrix(predicted))
  }
)


setMethod("stacked_loss", c("Surv", "matrix"),
  function(observed, predicted, times, ...) {
    modelmetrics(observed, predicted, times = times)["Brier"]
  }
)


setMethod("stacked_loss", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    -modelmetrics(observed, predicted)["CIndex"]
  }
)
