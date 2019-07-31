#' Selected Model
#' 
#' Model selected from a candidate set, as produced by the \code{\link{tune}}
#' function.
#' 
#' @param ... \code{MLModel} functions, function names, objects, or list of the
#' aforementioned elements in the candidate set from which to select.
#' @param control \code{\link{MLControl}} object, control function, or character
#' string naming a control function defining the resampling method to be
#' employed.
#' @param metrics function, one or more function names, or list of named
#' functions representing performance metrics to be calculated.  If not
#' specified, default metrics defined in the \code{\link{performance}}
#' functions are used.  Model selection is based on the first calculated metric.
#' @param stat function to compute a summary statistic on resampled values of
#' the metric for model selection.
#' @param cutoff argument passed to the \code{metrics} functions.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#' \code{Surv}
#' }
#' }
#' 
#' @return \code{SelectedModel} class object that inherits from \code{MLModel}.
#' 
#' @seealso \code{\link{tune}}, \code{\link{fit}}, \code{\link{resample}}
#' 
#' @examples
#' fit(sale_amount ~ ., data = ICHomes,
#'     model = SelectedModel(GBMModel, GLMNetModel, SVMRadialModel))
#' 
SelectedModel <- function(..., control = CVControl, metrics = NULL,
                          stat = base::mean, cutoff = NULL) {
  
  models <- unlist(list(...))
  control <- getMLObject(control, "MLControl")
  
  new("SelectedModel",
    name = "SelectedModel",
    label = "Selected Model",
    response_types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    predictor_encoding = NA_character_,
    params = params(environment())
  )
  
}


.fit.SelectedModel <- function(model, x, ...) {
  fit(x, model = do.call(tune, c(list(x), model@params)))
}
