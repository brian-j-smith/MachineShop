#' Super Learner Model
#'
#' Fit a super learner model to predictions from multiple base learners.
#'
#' @param ... \link[=models]{model} functions, function names, objects; other
#'   objects that can be \link[=as.MLModel]{coerced} to models; or vector of
#'   these to serve as base learners.
#' @param model \link[=models]{model} function, function name, or object
#'   defining the super model; or another object that can be
#'   \link[=as.MLModel]{coerced} to the model.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed for the estimation of base
#'   learner weights.
#' @param all_vars logical indicating whether to include the original
#'   predictor variables in the super model.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#'
#' @return \code{SuperModel} class object that inherits from \code{MLModel}.
#'
#' @references
#' van der Laan, M. J., Polley, E. C., & Hubbard, A. E. (2007). Super learner.
#' \emph{Statistical Applications in Genetics and Molecular Biology},
#' \emph{6}(1).
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested packages gbm and glmnet to run
#'
#' model <- SuperModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' model_fit <- fit(sale_amount ~ ., data = ICHomes, model = model)
#' predict(model_fit, newdata = ICHomes)
#' }
#'
SuperModel <- function(
  ..., model = GBMModel, control = MachineShop::settings("control"),
  all_vars = FALSE
) {

  models <- map(as.MLModel, unlist(list(model, ...)))
  names(models)[1] <- "SuperLearner"
  names(models)[-1] <- make_names_len(length(models) - 1, "BaseLearner")

  slots <- combine_model_slots(models, models[[1]]@response_types)
  new("SuperModel", MLModel(
    name = "SuperModel",
    label = "Super Learner",
    response_types = slots$response_types,
    weights = slots$weights,
    params = TrainingParams(
      optim = NullOptimization(),
      control = control,
      options = list(all_vars = all_vars)
    )
  ), candidates = ListOf(models))

}

MLModelFunction(SuperModel) <- NULL


.fit.SuperModel <- function(object, input, ...) {
  input_prep <- prep(input)
  mf <- ModelFrame(input_prep, na.rm = FALSE)

  super_learner <- object@candidates[[1]]
  base_learners <- object@candidates[-1]
  control <- object@params@control
  all_vars <- object@params@options$all_vars

  predictors <- list()
  ind <- new_progress_index(max = length(base_learners), name = object@name)
  while (ind < max(ind)) {
    ind <- ind + 1
    res <- resample(input, model = base_learners[[ind]], control = control,
                    progress_index = ind)
    predictors[[ind]] <- res$Predicted
  }

  df <- super_df(res$Observed, predictors, res$Case, if (all_vars) mf)
  super_mf <- ModelFrame(formula(df), df)

  list(base_fits = map(function(learner) fit(input, model = learner),
                       base_learners),
       super_fit = fit(super_mf, model = super_learner),
       all_vars = all_vars,
       times = control@predict$times) %>%
    MLModelFit("SuperModelFit", input = input_prep, model = object)
}


.predict.SuperModel <- function(object, model_fit, newdata, times, ...) {
  predictors <- map(function(fit) {
    predict(fit, newdata = newdata, times = model_fit$times, type = "default")
  }, model_fit$base_fits)

  df <- if (model_fit$all_vars) {
    newdata[["(names)"]] <- rownames(newdata)
    super_df(NA, predictors, newdata[["(names)"]], newdata)
  } else {
    super_df(NA, predictors)
  }

  predict(model_fit$super_fit, newdata = df, times = times, type = "default")
}


super_df <- function(y, predictors, case_names = NULL, data = NULL) {
  names(predictors) <- make_names_len(length(predictors), "X")
  df <- data.frame(y = y, unnest(as.data.frame(predictors)))

  if (!is.null(data)) {
    df[["(names)"]] <- case_names

    data_predictors <- predictors(data)
    unique_names <- make_unique(c(names(df), names(data_predictors)))
    names(data_predictors) <- tail(unique_names, length(data_predictors))
    data_predictors[["(names)"]] <- data[["(names)"]]

    merge(df, data_predictors, by = "(names)", sort = FALSE)[-1]
  } else {
    df
  }
}
