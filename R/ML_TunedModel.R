#' Tuned Model
#' 
#' Model tuned over a grid of parameter values, as produced by the
#' \code{\link{tune}} function.
#' 
#' @param model \code{MLModel} function, function name, or object to be tuned.
#' @param grid \code{data.frame} containing parameter values at which to
#' evaluate a single model supplied to \code{models}, the number of
#' parameter-specific values to generate automatically if the model has a
#' pre-defined grid, or a call to \code{\link{Grid}}.  Ignored in the case of a
#' list of models.
#' @param fixed list of fixed parameter values to combine with those in
#' \code{grid}.
#' @param control \code{\link{MLControl}} object, control function, or character
#' string naming a control function defining the resampling method to be
#' employed.
#' @param metrics function, function name, or vector of these with which to
#' calculate performance metrics.  If not specified, default metrics defined in
#' the \code{\link{performance}} functions are used.  Model selection is based
#' on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#' summary statistic on resampled metric values for model tuning.
#' @param cutoff argument passed to the \code{metrics} functions.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#' \code{Surv}
#' }
#' }
#' 
#' @return \code{TunedModel} class object that inherits from \code{MLModel}.
#' 
#' @seealso \code{\link{tune}}, \code{\link{fit}}, \code{\link{resample}}
#' 
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = TunedModel(GLMNetModel))
#' 
TunedModel <- function(model, grid = 3, fixed = NULL,
                       control = MachineShop::settings("control"),
                       metrics = NULL,
                       stat = MachineShop::settings("stat.ModelTune"),
                       cutoff = NULL) {
  
  if (missing(model)) model <- NULL
  control <- getMLObject(control, "MLControl")
  
  new("TunedModel",
    name = "TunedModel",
    label = "Grid Tuned Model",
    response_types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    predictor_encoding = NA_character_,
    params = params(environment())
  )
  
}


.fit.TunedModel <- function(model, x, ...) {
  fit(x, model = do.call(tune, c(list(x), model@params)))
}
