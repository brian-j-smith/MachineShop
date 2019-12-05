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

  base_learners <- lapply(unlist(list(...)), getMLObject, class = "MLModel")

  control <- getMLObject(control, "MLControl")

  new("SuperModel",
    name = "SuperModel",
    label = "Super Learner",
    response_types = .response_types,
    predictor_encoding = NA_character_,
    params = as.list(environment()),
    predict = function(object, newdata, times, ...) {
      predictors <- lapply(object$base_fits, function(fit) {
        predict(fit, newdata = newdata, times = object$times, type = "prob")
      })

      df <- super_df(NA, predictors, newdata[["(casenames)"]],
                     if (object$all_vars) newdata)

      predict(object$super_fit, newdata = df, times = times, type = "prob")
    },
    varimp = function(object, ...) NULL
  )

}

MLModelFunction(SuperModel) <- NULL


.fit.SuperModel <- function(model, x, ...) {
  mf <- ModelFrame(x, na.rm = FALSE)

  params <- model@params
  base_learners <- params$base_learners
  super_learner <- params$model
  control <- params$control

  predictors <- list()
  for (i in seq(base_learners)) {
    res <- resample(x, model = base_learners[[i]], control = control)
    predictors[[i]] <- res$Predicted
  }

  df <- super_df(res$Observed, predictors, res$Case, if (params$all_vars) mf)
  super_mf <- ModelFrame(formula(df), df)

  list(base_fits = lapply(base_learners,
                          function(learner) fit(mf, model = learner)),
       super_fit = fit(super_mf, model = super_learner),
       all_vars = params$all_vars,
       times = control@times) %>%
    MLModelFit("SuperModelFit", model, x, response(mf))
}


super_df <- function(y, predictors, casenames, data = NULL) {
  names(predictors) <- make.names(seq(predictors))
  df <- data.frame(y = y, unnest(as.data.frame(predictors)))

  if (!is.null(data)) {
    df[["(casenames)"]] <- casenames

    data_predictors <- predictors(data)
    unique_names <- make.unique(c(names(df), names(data_predictors)))
    names(data_predictors) <- tail(unique_names, length(data_predictors))
    data_predictors[["(casenames)"]] <- data[["(casenames)"]]

    merge(df, data_predictors, by = "(casenames)", sort = FALSE)[-1]
  } else {
    df
  }
}
