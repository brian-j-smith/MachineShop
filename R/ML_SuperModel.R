#' Super Learner Model
#' 
#' Fit a super learner model to predictions from multiple base learners.
#' 
#' @param ... \link[=models]{model} functions, function names, calls, or vector
#' of these to serve as base learners.
#' @param model \link[=models]{model} function, function name, or call defining
#' the super model.
#' @param control \link[=controls]{control} function, function name, or call
#' defining the resampling method to be employed for the estimation of base
#' learner weights.
#' @param all_vars logical indicating whether to include the original
#' predictor variables in the super model.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#' \code{Surv}
#' }
#' }
#' 
#' @return \code{SuperModel} class object that inherits from \code{MLModel}.
#' 
#' @references
#' 
#' van der Lann, M.J., Hubbard A.E. (2007) \emph{Super Learner.} Statistical
#' Applications in Genetics and Molecular Biology, 6(1).
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' model <- SuperModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' modelfit <- fit(sale_amount ~ ., data = ICHomes, model = model)
#' predict(modelfit, newdata = ICHomes)
#' 
SuperModel <- function(..., model = GBMModel,
                       control = MachineShop::settings("control"),
                       all_vars = FALSE) {
  
  base_learners <- lapply(unlist(list(...)), getMLObject, class = "MLModel")

  control <- getMLObject(control, "MLControl")

  new("SuperModel",
    name = "SuperModel",
    label = "Super Learner",
    response_types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    predictor_encoding = NA_character_,
    params = as.list(environment()),
    fitbits = MLFitBits(
      predict = function(object, newdata, times, ...) {
        learner_predictors <- lapply(object$base_fits, function(fit) {
          predict(fit, newdata = newdata, times = object$times, type = "prob")
        })
        
        df <- make_super_df(NA, learner_predictors, nrow(newdata))
        if (object$all_vars) df <- add_super_predictors(newdata, df)

        predict(object$super_fit, newdata = df, times = times, type = "prob")
      },
      varimp = function(object, ...) NULL
    )
  )

}


.fit.SuperModel <- function(model, x, ...) {
  mf <- ModelFrame(x, na.rm = FALSE)
  
  params <- model@params
  base_learners <- params$base_learners
  super_learner <- params$model
  control <- params$control

  learner_predictors <- list()
  for (i in seq(base_learners)) {
    response <- resample(x, model = base_learners[[i]], control = control)
    learner_predictors[[i]] <- response$Predicted
  }
  
  df <- make_super_df(response$Observed, learner_predictors, nrow(response))
  if (params$all_vars) df <- add_super_predictors(mf, df, response$Case)
  super_mf <- ModelFrame(formula(df), df)

  list(base_fits = lapply(base_learners,
                          function(learner) fit(mf, model = learner)),
       super_fit = fit(super_mf, model = super_learner),
       all_vars = params$all_vars,
       times = control@times) %>%
    asMLModelFit("SuperModelFit", model, x, response(mf))
}


make_super_df <- function(y, predictors, nrow) {
  predictors <- do.call(cbind, predictors)
  colnames(predictors) <- make.names(1:ncol(predictors))
  df <- data.frame(row.names = seq_len(nrow))
  df$y <- y
  cbind(df, predictors)
}


add_super_predictors <- function(from, to, casenames) {
  from_predictors <- predictors(from)
  
  unique_names <- make.unique(c(names(to), names(from_predictors)))
  names(from_predictors) <- tail(unique_names, length(from_predictors))

  if (missing(casenames)) {
    cbind(to, from_predictors)
  } else {
    case_name_var <- "(casenames)"
    from_predictors[[case_name_var]] <- from[[case_name_var]]
    to[[case_name_var]] <- casenames
    merge(to, from_predictors, by = case_name_var, sort = FALSE)[-1]
  }
}
