#' Display Model Information
#'
#' Display information about models supplied by the \pkg{MachineShop} package.
#'
#' @param ... \link[=models]{model} functions, function names, or calls;
#' \link[=response]{observed responses} for which to display information.  If
#' none are specified, information is returned on all available models by
#' default.
#'
#' @return List of named model elements each containing the following
#' components:
#' \describe{
#'   \item{label}{character descriptor for the model.}
#'   \item{packages}{character vector of source packages required to use the
#'     model.  These need only be installed with the
#'     \code{\link{install.packages}} function or by equivalent means; but need
#'     not be loaded with, for example, the \code{\link{library}} function.}
#'   \item{response_types}{character vector of response variable types supported
#'     by the model.}
#'   \item{arguments}{closure with the argument names and corresponding default
#'     values of the model function.}
#'   \item{grid}{logical indicating whether automatic generation of tuning
#'     parameter grids is implemented for the model.}
#'   \item{varimp}{logical indicating whether variable importance is defined for
#'     the model.}
#' }
#'
#' @examples
#' ## All models
#' modelinfo()
#'
#' ## Models by response types
#' names(modelinfo(factor(0)))
#' names(modelinfo(factor(0), numeric(0)))
#'
#' ## Model-specific information
#' modelinfo(GBMModel)
#'
modelinfo <- function(...) {
  args <- list(...)
  args <- if (length(args)) unname(args) else as.list(.model_names)
  info <- do.call(.modelinfo, args)

  is_type <- if (length(info)) !mapply(is, info, "list") else NULL
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
                  "BARTModel",
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
                  "SelectedModel",
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
                  "TunedModel",
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
    response_types = x@response_types,
    arguments = args(get0(x@name, mode = "function")),
    grid = !is.null(body(x@grid)),
    varimp = !is.null(body(x@varimp))
  )), names = x@name)
  if (length(list(...))) c(info, .modelinfo(...)) else info
}


.modelinfo_types <- function(...) {
  info <- modelinfo()
  is_supported <- sapply(info, function(this) {
    all(sapply(list(...), function(object) {
      any(mapply(is_response, list(object), this$response_types))
    }))
  })
  info[is_supported]
}
