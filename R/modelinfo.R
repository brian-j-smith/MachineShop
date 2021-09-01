#' Display Model Information
#'
#' Display information about models supplied by the \pkg{MachineShop} package.
#'
#' @param ... \link[=models]{model} functions, function names, or objects;
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
#'  \item{weights}{logical value or vector of the same length as
#'    \code{response_types} indicating whether case weights are supported for
#'    the responses.}
#'   \item{arguments}{closure with the argument names and corresponding default
#'     values of the model function.}
#'   \item{grid}{logical indicating whether automatic generation of tuning
#'     parameter grids is implemented for the model.}
#'   \item{varimp}{logical indicating whether model-specific variable importance
#'     is defined.}
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
  args <- if (length(args)) unname(args) else as.list(settings("models"))
  info <- do.call(.modelinfo, args)

  is_type <- if (length(info)) !map_logi(is, info, "list") else NULL
  if (any(is_type)) {
    info_models <- if (all(is_type)) modelinfo() else info[!is_type]
    info_types <- do.call(.modelinfo_types, info[is_type])
    info <- c(info_models, info_types)
    info <- info[intersect(names(info_models), names(info_types))]
  }

  info[unique(names(info))]
}


.modelinfo <- function(x, ...) {
  UseMethod(".modelinfo")
}


.modelinfo.default <- function(x, ...) {
  info <- list(x)
  if (...length()) c(info, .modelinfo(...)) else info
}


.modelinfo.character <- function(x, ...) {
  model <- try(get_MLModel(x), silent = TRUE)
  if (is(model, "try-error")) model <- list()
  .modelinfo(model, ...)
}


.modelinfo.function <- function(x, ...) {
  model <- try(get_MLModel(x), silent = TRUE)
  if (is(model, "try-error")) model <- list()
  .modelinfo(model, ...)
}


.modelinfo.list <- function(x, ...) {
  if (...length()) .modelinfo(...) else list()
}


.modelinfo.MLModel <- function(x, ...) {
  info <- structure(list(list(
    label = x@label,
    packages = x@packages,
    response_types = x@response_types,
    weights = x@weights,
    arguments = args(get0(x@name, mode = "function")),
    grid = has_grid(x),
    varimp = has_varimp(x)
  )), names = x@name)
  if (...length()) c(info, .modelinfo(...)) else info
}


.modelinfo_types <- function(...) {
  info <- modelinfo()
  check_model <- function(model) {
    check_response <- function(y) any(is_response(y, model$response_types))
    all(map_logi(check_response, list(...)))
  }
  info[map_logi(check_model, info)]
}
