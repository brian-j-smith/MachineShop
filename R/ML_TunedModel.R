#' Tuned Model
#' 
#' A model tuned over a grid of parameter values or selected from a list
#' of candidate models as implemented with the \code{\link{tune}} function.
#' 
#' @param models \code{MLModel} function, function name, object or list of the
#' aforementioned elements, such as that returned by \code{\link{expand_model}}.
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
#' @param metrics function, one or more function names, or list of named
#' functions to include in the calculation of performance metrics.  The default
#' \code{\link{performance}} metrics are used unless otherwise specified.  Model
#' selection is based on the first specified metric.
#' @param stat function to compute a summary statistic on resampled values of
#' the metric for model selection.
#' @param maximize logical indicating whether to select the model having the
#' maximum or minimum value of the performance metric.  Set automatically if a
#' package \code{\link{metrics}} function is explicitly specified for the model
#' selection.
#' @param ... arguments passed to the \code{metrics} functions.
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
TunedModel <- function(models = NULL, grid = 3, fixed = NULL,
                       control = CVControl, metrics = NULL, stat = base::mean,
                       maximize = TRUE, ...) {
  
  control <- getMLObject(control, "MLControl")
  
  new("TunedModel",
    name = "TunedModel",
    label = "Tuned Model",
    response_types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    predictor_encoding = NA_character_,
    params = params(environment())
  )
  
}


.fit.TunedModel <- function(model, x, ...) {
  fit(x, model = do.call(tune, c(list(x), model@params)))
}
