#' Selected Model
#' 
#' Model selection from a candidate set.
#' 
#' @param ... \link[=models]{model} functions, function names, calls, or vectors
#'   of these to serve as the candidate set from which to select, such as that
#'   returned by \code{\link{expand_model}}.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Model
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for model selection.
#' @param cutoff argument passed to the \code{metrics} functions.
#' 
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordered},
#'     \code{Surv}}
#' }
#' 
#' @return \code{SelectedModel} class object that inherits from \code{MLModel}.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(sale_amount ~ ., data = ICHomes,
#'     model = SelectedModel(GBMModel, GLMNetModel, SVMRadialModel))
#' 
SelectedModel <- function(..., control = MachineShop::settings("control"),
                          metrics = NULL,
                          stat = MachineShop::settings("stat.Tune"),
                          cutoff = MachineShop::settings("cutoff")) {
  
  models <- as.list(unlist(list(...)))
  model_names <- character()
  for (i in seq(models)) {
    models[[i]] <- getMLObject(models[[i]], class = "MLModel")
    name <- names(models)[i]
    model_names[i] <- 
      if (!is.null(name) && nzchar(name)) name else models[[i]]@name
  }
  names(models) <- make.unique(model_names)
  
  new("SelectedModel",
    name = "SelectedModel",
    label = "Selected Model",
    response_types = c("factor", "matrix", "numeric", "ordered", "Surv"),
    predictor_encoding = NA_character_,
    params = list(models = models, control = getMLObject(control, "MLControl"),
                  metrics = metrics, stat = stat, cutoff = cutoff)
  )
  
}

MLModelFunction(SelectedModel) <- NULL


.fit.SelectedModel <- function(model, x, ...) {
  fit(x, model = tune(model, x))
}
