#' Display Model Information
#' 
#' Display information about models provided by the \pkg{MachineShop} package.
#' 
#' @param ... \code{MLModel} objects, constructor functions, character
#' strings naming constructor functions, or supported responses for which to
#' display information.  If none are specified, information is returned on all
#' available models by default.
#' 
#' @return List of named models containing the \code{"label"}, required
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
  .modelinfo(...)
}


.model_names <- c("AdaBagModel",
                  "AdaBoostModel",
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


setGeneric(".modelinfo", function(x, ...) standardGeneric(".modelinfo"))


setMethod(".modelinfo", "missing",
  function(x, ...) {
    do.call(modelinfo, as.list(.model_names))
  }
)


setMethod(".modelinfo", "ANY",
  function(x, ...) {
    args <- list(x, ...)
    info <- modelinfo()
    is_type <- sapply(info, function(this) {
      all(sapply(args, function(object) {
        any(sapply(this$types, function(type) is_response(object, type)))
      }))
    })
    info[is_type]
  }
)


setMethod(".modelinfo", "character",
  function(x, ...) {
    modelinfo(getMLObject(x, "MLModel"), ...)
  }
)


setMethod(".modelinfo", "function",
  function(x, ...) {
    modelinfo(getMLObject(x, "MLModel"), ...)
  }
)


setMethod(".modelinfo", "MLModel",
  function(x, ...) {
    info <- structure(list(list(
      label = x@label,
      packages = x@packages,
      types = x@types,
      arguments = args(get(x@name, mode = "function")),
      varimp = !is.null(body(fitbit(x, "varimp")))
    )), names = x@name)
    if (length(list(...))) {
      info <- c(info, modelinfo(...))
      info <- info[unique(names(info))]
    }
    info
  }
)
