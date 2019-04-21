#' Stacked Regression Model
#' 
#' Fit a stacked regression model from multiple base learners.
#' 
#' @param ... \code{MLModel} objects to serve as base learners.
#' @param control \code{\link{MLControl}} object, control function, or character
#' string naming a control function defining the resampling method to be
#' employed for the estimation of base learner weights.
#' @param weights optional fixed base learner weights.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#' \code{Surv}
#' }
#' }
#' 
#' @return \code{StackedModel} class object that inherits from \code{MLModel}.
#' 
#' @references
#' Breiman, L. (1996) \emph{Stacked Regression.} Machine Learning, 24, 49--64.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' model <- StackedModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' modelfit <- fit(sale_amount ~ ., data = ICHomes, model = model)
#' predict(modelfit, newdata = ICHomes)
#' 
StackedModel <- function(..., control = CVControl, weights = NULL) {
  
  base_learners <- lapply(list(...), getMLObject, class = "MLModel")
  
  control <- getMLObject(control, "MLControl")

  if (!is.null(weights)) stopifnot(length(weights) == length(base_learners))
  
  new("StackedModel",
    name = "StackedModel",
    label = "Stacked Regression",
    types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    params = as.list(environment()),
    fitbits = MLFitBits(
      predict = function(object, newdata, ...) {
        predicted <- 0
        for (i in seq(object$base_fits)) {
          predicted <- predicted +
            object$weights[i] * predict(object$base_fits[[i]],
                                        newdata = newdata,
                                        times = object$times, type = "prob")
        }
        predicted
      },
      varimp = function(object, ...) NULL
    )
  )
  
}


setClass("StackedModel", contains = "MLModel")


.fit.StackedModel <- function(model, x, ...) {
  mf <- ModelFrame(x, na.rm = FALSE)
  
  base_learners <- model@params$base_learners
  weights <- model@params$weights
  control <-  model@params$control
  times <- control@times
  
  if (is.null(weights)) {
    num_learners <- length(base_learners)
    stack <- list()
    complete_responses <- TRUE
    for (i in 1:num_learners) {
      stack[[i]] <- resample(x, model = base_learners[[i]], control = control)
      complete_responses <-
        complete_responses & complete.cases(stack[[i]])
    }
    
    stack <- lapply(stack, function(response) {
      response[complete_responses, , drop = FALSE]
    })

    weights <- Rsolnp::solnp(rep(1 / num_learners, num_learners),
                             function(x) eval_stack_loss(x, stack, times),
                             eqfun = function(x) sum(x), eqB = 1,
                             LB = rep(0, num_learners),
                             control = list(trace = FALSE))$pars
  }
  
  list(base_fits = lapply(base_learners,
                          function(learner) fit(mf, model = learner)),
       weights = weights,
       times = times) %>%
    asMLModelFit("StackedModelFit", model, x, response(mf))
}


eval_stack_loss <- function(weights, responses, times) {
  predicted <- 0
  for (i in seq(responses)) {
    predicted <- predicted + weights[i] * responses[[i]]$Predicted
  }
  df <- responses[[1]]["Observed"]
  df$Predicted <- predicted
  by(df, responses[[1]]$Resample, function(x) {
    stack_loss(x$Observed, x$Predicted, times = times)
  }) %>% mean(na.rm = TRUE)
}


setGeneric("stack_loss",
           function(observed, predicted, ...) standardGeneric("stack_loss"))


setMethod("stack_loss", c("factor", "matrix"),
  function(observed, predicted, ...) {
    stack_loss(model.matrix(~ observed - 1), predicted)
  }
)


setMethod("stack_loss", c("factor", "numeric"),
  function(observed, predicted, ...) {
    stack_loss(observed, cbind(1 - predicted, predicted))
  }
)


setMethod("stack_loss", c("matrix", "matrix"),
  function(observed, predicted, ...) {
    stopifnot(ncol(observed) == ncol(predicted))
    sum((observed - predicted)^2) / nrow(observed)
  }
)


setMethod("stack_loss", c("numeric", "numeric"),
  function(observed, predicted, ...) {
    stack_loss(as.matrix(observed), as.matrix(predicted))
  }
)


setMethod("stack_loss", c("Surv", "matrix"),
  function(observed, predicted, times, ...) {
    brier(observed, predicted, times)[1]
  }
)


setMethod("stack_loss", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    -cindex(observed, predicted)
  }
)
