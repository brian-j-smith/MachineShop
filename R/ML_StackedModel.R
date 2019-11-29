#' Stacked Regression Model
#'
#' Fit a stacked regression model from multiple base learners.
#'
#' @param ... \link[=models]{model} functions, function names, calls, or vector
#'   of these to serve as base learners.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed for the estimation of base
#'   learner weights.
#' @param weights optional fixed base learner weights.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#'
#' @return \code{StackedModel} class object that inherits from \code{MLModel}.
#'
#' @references
#' Breiman, L. (1996) \emph{Stacked Regression.} Machine Learning, 24, 49--64.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' model <- StackedModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' model_fit <- fit(sale_amount ~ ., data = ICHomes, model = model)
#' predict(model_fit, newdata = ICHomes)
#'
StackedModel <- function(..., control = MachineShop::settings("control"),
                         weights = NULL) {

  base_learners <- lapply(unlist(list(...)), getMLObject, class = "MLModel")

  control <- getMLObject(control, "MLControl")

  if (!is.null(weights)) stopifnot(length(weights) == length(base_learners))

  new("StackedModel",
    name = "StackedModel",
    label = "Stacked Regression",
    response_types = .response_types,
    predictor_encoding = NA_character_,
    params = as.list(environment()),
    predict = function(object, newdata, ...) {
      pred <- 0
      for (i in seq(object$base_fits)) {
        base_pred <- predict(object$base_fits[[i]], newdata = newdata,
                             times = object$times, type = "prob")
        pred <- pred + object$weights[i] * base_pred
      }
      pred
    },
    varimp = function(object, ...) NULL
  )

}

MLModelFunction(StackedModel) <- NULL


.fit.StackedModel <- function(model, x, ...) {
  mf <- ModelFrame(x, na.rm = FALSE)

  base_learners <- model@params$base_learners
  weights <- model@params$weights
  control <-  model@params$control

  if (is.null(weights)) {
    num_learners <- length(base_learners)
    stack <- list()
    complete_cases <- TRUE
    for (i in 1:num_learners) {
      stack[[i]] <- resample(x, model = base_learners[[i]], control = control)
      complete_cases <- complete_cases & complete.cases(stack[[i]])
    }
    stack <- lapply(stack, function(res) res[complete_cases, ])

    weights <- Rsolnp::solnp(rep(1 / num_learners, num_learners),
                             function(weights) mean_stack_list(stack, weights),
                             eqfun = function(x) sum(x), eqB = 1,
                             LB = rep(0, num_learners),
                             control = list(trace = FALSE))$pars
  }

  list(base_fits = lapply(base_learners,
                          function(learner) fit(mf, model = learner)),
       weights = weights,
       times = control@times) %>%
    MLModelFit("StackedModelFit", model, x, response(mf))
}


mean_stack_list <- function(x, weights) {
  pred <- 0
  for (i in seq(x)) pred <- pred + weights[i] * x[[i]]$Predicted
  res <- x[[1]]
  res$Predicted <- pred
  mean(stack_loss(res$Observed, res$Predicted, res), na.rm = TRUE)
}


setGeneric("stack_loss",
           function(observed, predicted, x, ...) standardGeneric("stack_loss"))


setMethod("stack_loss", c("ANY", "ANY", "Resamples"),
  function(observed, predicted, x, ...) mse(x)
)


setMethod("stack_loss", c("factor", "ANY", "Resamples"),
  function(observed, predicted, x, ...) brier(x)
)


setMethod("stack_loss", c("Surv", "numeric", "Resamples"),
  function(observed, predicted, x, ...) -cindex(x)
)


setMethod("stack_loss", c("Surv", "SurvProbs", "Resamples"),
  function(observed, predicted, x, ...) brier(x)[, 1]
)
