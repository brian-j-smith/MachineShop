#' Stacked Regression Model
#'
#' Fit a stacked regression model from multiple base learners.
#'
#' @param ... \link[=models]{model} functions, function names, objects, or
#'   vector of these to serve as base learners.
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
#' Breiman, L. (1996) \emph{Stacked Regression.} Machine Learning, 24, 49--64.
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

  base_learners <- ListOf(map(get_MLModel, unlist(list(...))))
  names(base_learners) <- paste0(if (length(base_learners)) "Learner",
                                 seq_along(base_learners))

  control <- get_MLControl(control)

  if (!is.null(weights)) stopifnot(length(weights) == length(base_learners))

  slots <- combine_modelslots(base_learners, settings("response_types"))
  new("StackedModel",
    name = "StackedModel",
    label = "Stacked Regression",
    response_types = slots$response_types,
    weights = slots$weights,
    predictor_encoding = NA_character_,
    params = as.list(environment()),
    varimp = function(object, ...) NULL
  )

}

MLModelFunction(StackedModel) <- NULL


.fit.StackedModel <- function(x, inputs, ...) {
  base_learners <- x@params$base_learners
  weights <- x@params$weights
  control <-  x@params$control

  if (is.null(weights)) {
    num_learners <- length(base_learners)
    stack <- list()
    complete_cases <- TRUE
    i <- new_progress_index(names = x@name, max = num_learners)
    while (i < max(i)) {
      i <- i + 1
      stack[[i]] <- resample(inputs, model = base_learners[[i]],
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

  list(base_fits = map(function(learner) fit(inputs, model = learner),
                       base_learners),
       weights = weights,
       times = control@predict$times) %>%
    MLModelFit("StackedModelFit", model = x, x = inputs)
}


.predict.StackedModel <- function(x, object, newdata, ...) {
  pred <- 0
  for (i in seq_along(object$base_fits)) {
    base_pred <- predict(object$base_fits[[i]], newdata = newdata,
                         times = object$times, type = "prob")
    pred <- pred + object$weights[i] * base_pred
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
