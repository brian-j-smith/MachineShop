#' Super Learner Model
#' 
#' Fit a super learner model to predictions from multiple base learners.
#' 
#' @param ... \code{MLModel} objects to serve as base learners.
#' @param model \code{MLModel} object, constructor function, or character string
#' naming a constructor function to serve as the super model.
#' @param control \code{\linkS4class{MLControl}} object, control function, or
#' character string naming a control function defining the resampling method to
#' be employed for the estimation of base learner weights.
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
#' library(MASS)
#' 
#' model <- SuperModel(GBMModel, SVMRadialModel, GLMNetModel(lambda = 0.01))
#' modelfit <- fit(medv ~ ., data = Boston, model = model)
#' predict(modelfit, newdata = Boston)
#' 
SuperModel <- function(..., model = GBMModel, control = CVControl,
                       all_vars = FALSE) {
  
  base_learners <- lapply(list(...), getMLObject, class = "MLModel")

  control <- getMLObject(control, "MLControl")
  control@summary <- function(observed, predicted, ...) NA
  
  new("SuperModel",
    name = "SuperModel",
    types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    params = as.list(environment()),
    fitbits = MLFitBits(
      predict = function(object, newdata, times, ...) {
        newdata <- ModelFrame(formula(object), newdata, na.action = na.pass)
        
        learner_predictors <- lapply(object$base_fits, function(fit) {
          predict(fit, newdata = newdata, times = object$times, type = "prob")
        })
        df <- make_super_df(NA, learner_predictors, row.names(newdata))
  
        mf <- ModelFrame(formula(df), df, na.action = na.pass)
        if (object$all_vars) mf <- add_predictors(newdata, mf)
        
        predict(object$super_fit, newdata = mf, times = times, type = "prob")
      },
      varimp = function(object, ...) NULL
    )
  )

}


setClass("SuperModel", contains = "MLModel")


.fit.SuperModel <- function(model, x, ...) {
  mf <- ModelFrame(x)
  
  params <- model@params
  base_learners <- params$base_learners
  super_learner <- params$model
  control <- params$control

  learner_predictors <- list()
  for (i in seq(base_learners)) {
    response <- resample(x, model = base_learners[[i]], control = control)
    learner_predictors[[i]] <- response$Predicted
  }
  df <- make_super_df(response$Observed, learner_predictors, response$Case)
  na.action <- ifelse(control@na.rm, na.omit, na.pass)
  super_mf <- ModelFrame(formula(df), df, na.action = na.action)
  if (params$all_vars) super_mf <- add_predictors(mf, super_mf)

  list(base_fits = lapply(base_learners,
                          function(learner) fit(mf, model = learner)),
       super_fit = fit(super_mf, model = super_learner),
       all_vars = params$all_vars,
       times = control@surv_times) %>%
    asMLModelFit("SuperModelFit", model, x, response(mf))
}


make_super_df <- function(y, predictors, row.names) {
  predictors <- do.call(cbind, predictors)
  colnames(predictors) <- make.names(1:ncol(predictors))
  df <- data.frame(matrix(nrow = nrow(predictors), ncol = 0),
                   row.names = paste(seq(row.names), row.names, sep = "."))
  df$y <- y
  cbind(df, predictors)
}


add_predictors <- function(from, to) {
  from_terms <- terms(from)
  from <- get_all_vars(formula(from_terms)[-2], from)

  lhs <- names(to)[1]
  rhs <- c(labels(from_terms), names(to)[-1])
  
  from[["(row.names)"]] <- row.names(from)
  to[["(row.names)"]] <- sub(".+?[.]", "", row.names(to))
  data <- merge(from, to, by = "(row.names)", sort = FALSE)
  data[["(row.names)"]] <- NULL

  ModelFrame(reformulate(rhs, lhs), data, na.action = na.pass)
}
