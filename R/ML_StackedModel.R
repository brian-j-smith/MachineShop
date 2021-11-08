#' Stacked Regression Model
#'
#' Fit a stacked regression model from multiple base learners.
#'
#' @param ... \link[=models]{model} functions, function names, objects; other
#'   objects that can be \link[=as.MLModel]{coerced} to models; or vector of
#'   these to serve as base learners.
#' @param control \link[=controls]{control} function, function name, or object
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
#' Breiman, L. (1996). Stacked regression. \emph{Machine Learning}, \emph{24},
#' 49-64.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested packages gbm and glmnet to run
#'
#' model <- StackedModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' model_fit <- fit(sale_amount ~ ., data = ICHomes, model = model)
#' predict(model_fit, newdata = ICHomes)
#' }
#'
StackedModel <- function(
  ..., control = MachineShop::settings("control"), weights = NULL
) {

  base_learners <- ListOf(map(as.MLModel, unlist(list(...))))
  names(base_learners) <- paste0(if (length(base_learners)) "Learner",
                                 seq_along(base_learners))

  control <- as.MLControl(control)
  if (length(weights)) stopifnot(length(weights) == length(base_learners))
  params <- as.list(environment())

  slots <- combine_model_slots(base_learners, settings("response_types"))
  new("StackedModel", MLModel(
    name = "StackedModel",
    label = "Stacked Regression",
    response_types = slots$response_types,
    weights = slots$weights,
    params = params
  ))

}

MLModelFunction(StackedModel) <- NULL


.fit.StackedModel <- function(object, input, ...) {
  base_learners <- object@params$base_learners
  weights <- object@params$weights
  control <-  object@params$control

  if (is.null(weights)) {
    num_learners <- length(base_learners)
    stack <- list()
    complete_cases <- TRUE
    i <- new_progress_index(names = object@name, max = num_learners)
    while (i < max(i)) {
      i <- i + 1
      stack[[i]] <- resample(input, model = base_learners[[i]],
                             control = control, progress_index = i)
      complete_cases <- complete_cases & complete.cases(stack[[i]])
    }
    stack <- map(function(res) res[complete_cases, ], stack)

    weights <- Rsolnp::solnp(rep(1 / num_learners, num_learners),
                             function(weights) mean_stack_list(stack, weights),
                             eqfun = function(x) sum(x), eqB = 1,
                             LB = numeric(num_learners),
                             control = list(trace = FALSE))$pars
  }

  list(base_fits = map(function(learner) fit(input, model = learner),
                       base_learners),
       weights = weights,
       times = control@predict$times) %>%
    MLModelFit("StackedModelFit", model = object, input = input)
}


.predict.StackedModel <- function(object, model_fit, newdata, ...) {
  pred <- 0
  for (i in seq_along(model_fit$base_fits)) {
    base_pred <- predict(model_fit$base_fits[[i]], newdata = newdata,
                         times = model_fit$times, type = "prob")
    pred <- pred + model_fit$weights[i] * base_pred
  }
  pred
}


mean_stack_list <- function(x, weights) {
  pred <- 0
  for (i in seq_along(x)) pred <- pred + weights[i] * x[[i]]$Predicted
  res <- x[[1]]
  res$Predicted <- pred
  mean(stack_loss(res$Observed, res$Predicted, res), na.rm = TRUE)
}


setGeneric("stack_loss",
  function(observed, predicted, x, ...) standardGeneric("stack_loss")
)


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
