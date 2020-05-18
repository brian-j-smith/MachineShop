#' Super Learner Model
#'
#' Fit a super learner model to predictions from multiple base learners.
#'
#' @param ... \link[=models]{model} functions, function names, calls, or vector
#'   of these to serve as base learners.
#' @param model \link[=models]{model} function, function name, or call defining
#'   the super model.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed for the estimation of base
#'   learner weights.
#' @param all_vars logical indicating whether to include the original
#'   predictor variables in the super model.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#'
#' @return \code{SuperModel} class object that inherits from \code{MLModel}.
#'
#' @references
#'
#' van der Lann, M.J., Hubbard A.E. (2007) \emph{Super Learner.} Statistical
#' Applications in Genetics and Molecular Biology, 6(1).
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' model <- SuperModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' model_fit <- fit(sale_amount ~ ., data = ICHomes, model = model)
#' predict(model_fit, newdata = ICHomes)
#'
SuperModel <- function(..., model = GBMModel,
                       control = MachineShop::settings("control"),
                       all_vars = FALSE) {

  base_learners <- ListOf(map(getMLObject, unlist(list(...)), "MLModel"))
  names(base_learners) <- paste0(if (length(base_learners)) "Learner",
                                 seq(base_learners))

  control <- getMLObject(control, "MLControl")

  new("SuperModel",
    name = "SuperModel",
    label = "Super Learner",
    response_types =
      Reduce(intersect, map(slot, base_learners, "response_types"),
             init = getMLObject(model, "MLModel")@response_types),
    predictor_encoding = NA_character_,
    params = as.list(environment()),
    varimp = function(object, ...) NULL
  )

}

MLModelFunction(SuperModel) <- NULL


.fit.SuperModel <- function(x, inputs, ...) {
  inputs_prep <- prep(inputs)
  mf <- ModelFrame(inputs_prep, na.rm = FALSE)

  params <- x@params
  base_learners <- params$base_learners
  super_learner <- params$model
  control <- params$control

  predictors <- list()
  i <- structure(0, max = length(base_learners), names = class(x))
  while (i < attr(i, "max")) {
    i <- i + 1
    res <- resample(inputs, model = base_learners[[i]], control = control,
                    progress_index = i)
    predictors[[i]] <- res$Predicted
  }

  df <- super_df(res$Observed, predictors, res$Case, if (params$all_vars) mf)
  super_mf <- ModelFrame(formula(df), df)

  list(base_fits = map(function(learner) fit(inputs, model = learner),
                       base_learners),
       super_fit = fit(super_mf, model = super_learner),
       all_vars = params$all_vars,
       times = control@times) %>%
    MLModelFit("SuperModelFit", model = x, x = inputs_prep)
}


.predict.SuperModel <- function(x, object, newdata, times, ...) {
  predictors <- map(function(fit) {
    predict(fit, newdata = newdata, times = object$times, type = "prob")
  }, object$base_fits)

  df <- if (object$all_vars) {
    newdata <- preprocess(x@x, newdata)
    newdata[["(names)"]] <- rownames(newdata)
    super_df(NA, predictors, newdata[["(names)"]], newdata)
  } else {
    super_df(NA, predictors)
  }

  predict(object$super_fit, newdata = df, times = times, type = "prob")
}


super_df <- function(y, predictors, casenames = NULL, data = NULL) {
  names(predictors) <- make.names(seq(predictors))
  df <- data.frame(y = y, unnest(as.data.frame(predictors)))

  if (!is.null(data)) {
    df[["(names)"]] <- casenames

    data_predictors <- predictors(data)
    unique_names <- make.unique(c(names(df), names(data_predictors)))
    names(data_predictors) <- tail(unique_names, length(data_predictors))
    data_predictors[["(names)"]] <- data[["(names)"]]

    merge(df, data_predictors, by = "(names)", sort = FALSE)[-1]
  } else {
    df
  }
}
