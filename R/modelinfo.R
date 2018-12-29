#' Display Model Information
#' 
#' Display information about models provided by the \pkg{MachineShop} package.
#' 
#' @param ... \code{MLModel} objects, constructor functions, constructor
#' function names, or supported responses for which to display information.  If
#' none are specified, information is returned on all available models by
#' default.
#' 
#' @return List of named models containing a descriptive \code{"label"}, source
#' \code{"packages"}, supported response variable \code{"types"}, the
#' constructor \code{"arguments"}, and whether a \code{"varimp"} function is
#' implemented for each.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' ## All models
#' modelinfo()
#' 
#' ## Models by response types
#' names(modelinfo(factor(0)))
#' names(modelinfo(factor(0), numeric(0)))
#' 
modelinfo <- function(...) {
  args <- list(...)
  if (length(args) == 0) args <- as.list(.model_names)
  info <- do.call(.modelinfo, args)
  
  is_type <- !sapply(info, is, class2 = "list")
  if (any(is_type)) {
    info_models <- if (all(is_type)) modelinfo() else info[!is_type]
    info_types <- do.call(.modelinfo_types, info[is_type])
    info <- c(info_models, info_types)
    info <- info[intersect(names(info_models), names(info_types))]
  }
  
  info[unique(names(info))]
}


.model_names <- c("AdaBagModel",
                  "AdaBoostModel",
                  "BARTMachineModel",
                  "BlackBoostModel",
                  "C50Model",
                  "CForestModel",
                  "CoxModel",
                  "CoxStepAICModel",
                  "EarthModel",
                  "FDAModel",
                  "GAMBoostModel",
                  "GBMModel",
                  "GLMBoostModel",
                  "GLMModel",
                  "GLMStepAICModel",
                  "GLMNetModel",
                  "KNNModel",
                  "LARSModel",
                  "LDAModel",
                  "LMModel",
                  "MDAModel",
                  "NaiveBayesModel",
                  "NNetModel",
                  "PDAModel",
                  "PLSModel",
                  "POLRModel",
                  "QDAModel",
                  "RandomForestModel",
                  "RangerModel",
                  "RPartModel",
                  "StackedModel",
                  "SuperModel",
                  "SurvRegModel",
                  "SurvRegStepAICModel",
                  "SVMModel",
                  "SVMANOVAModel",
                  "SVMBesselModel",
                  "SVMLaplaceModel",
                  "SVMLinearModel",
                  "SVMPolyModel",
                  "SVMRadialModel",
                  "SVMSplineModel",
                  "SVMTanhModel",
                  "TreeModel",
                  "XGBModel",
                  "XGBDARTModel",
                  "XGBLinearModel",
                  "XGBTreeModel")


.modelinfo <- function(x, ...) {
  UseMethod(".modelinfo")
}


.modelinfo.default <- function(x, ...) {
  info <- list(x)
  if (length(list(...))) c(info, .modelinfo(...)) else info
}


.modelinfo.character <- function(x, ...) {
  model <- try(getMLObject(x, "MLModel"), silent = TRUE)
  if (is(model, "try-error")) model <- list()
  .modelinfo(model, ...)
}


.modelinfo.function <- function(x, ...) {
  model <- try(getMLObject(x, "MLModel"), silent = TRUE)
  if (is(model, "try-error")) model <- list()
  .modelinfo(model, ...)
}


.modelinfo.list <- function(x, ...) {
  if (length(list(...))) .modelinfo(...) else list()
}


.modelinfo.MLModel <- function(x, ...) {
  info <- structure(list(list(
    label = x@label,
    packages = x@packages,
    types = x@types,
    arguments = args(get(x@name)),
    varimp = !is.null(body(fitbit(x, "varimp")))
  )), names = x@name)
  if (length(list(...))) c(info, .modelinfo(...)) else info
}


.modelinfo_types <- function(...) {
  info <- modelinfo()
  is_supported <- sapply(info, function(this) {
    all(sapply(list(...), function(object) {
      any(mapply(is_response, list(object), this$types))
    }))
  })
  info[is_supported]
}
