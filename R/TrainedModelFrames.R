#' Selected Model Frame
#'
#' Formula, design matrix, or model frame selection from a candidate set.
#'
#' @name SelectedModelFrame
#' @rdname SelectedModelFrame
#'
#' @param ... \code{\link{formula}}, design \code{\link{matrix}} of predictors,
#'   or \code{\link{ModelFrame}} objects, or a list of one of these types.
#' @param y response variable.
#' @param data \link[=data.frame]{data frame} or an object that can be converted
#'   to one.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Recipe
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe selection.
#' @param cutoff argument passed to the \code{metrics} functions.
#'
#' @return \code{SelectedModelFrame} class object that inherits from
#' \code{ModelFrame}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' sel_fo <- SelectedFormula(
#'   sale_amount ~ sale_year + built + style + construction,
#'   sale_amount ~ sale_year + base_size + bedrooms + basement,
#'   data = ICHomes
#' )
#'
#' model_fit <- fit(sel_fo, model = GLMModel)
#' (selected_model <- as.MLModel(model_fit))
#' plot(selected_model)
#'
NULL


#' @rdname SelectedModelFrame
#'
SelectedFormula <- function(..., data,
                            control = MachineShop::settings("control"),
                            metrics = NULL,
                            stat = MachineShop::settings("stat.Train"),
                            cutoff = MachineShop::settings("cutoff")) {
  args <- list(...)
  if (is_one_element(args, "list")) args <- args[[1]]
  if (!all(mapply(is, args, "formula"))) stop("arguments must be formulas")
  SelectedModelFrame(mapply(ModelFrame, args, list(data),
                            MoreArgs = list(na.rm = FALSE), SIMPLIFY = FALSE),
                     control = control, metrics = metrics, stat = stat,
                     cutoff = cutoff)
}


#' @rdname SelectedModelFrame
#'
SelectedMatrix <- function(..., y,
                            control = MachineShop::settings("control"),
                            metrics = NULL,
                            stat = MachineShop::settings("stat.Train"),
                            cutoff = MachineShop::settings("cutoff")) {
  args <- list(...)
  if (is_one_element(args, "list")) args <- args[[1]]
  if (!all(mapply(is, args, "matrix"))) stop("arguments must be matrices")
  SelectedModelFrame(mapply(ModelFrame, args, list(y),
                            MoreArgs = list(na.rm = FALSE), SIMPLIFY = FALSE),
                     control = control, metrics = metrics, stat = stat,
                     cutoff = cutoff)
}


#' @rdname SelectedModelFrame
#'
SelectedModelFrame <- function(..., control = MachineShop::settings("control"),
                               metrics = NULL,
                               stat = MachineShop::settings("stat.Train"),
                               cutoff = MachineShop::settings("cutoff")) {

  args <- list(...)
  if (is_one_element(args, "list") && !is(args, "ModelFrame")) {
    args <- args[[1]]
  }
  names(args) <- make_list_names(args, "ModelFrame")

  response <- args[[1]][[1]]
  is_valid <- function(x) is(x, "ModelFrame") && identical(x[[1]], response)
  if (!all(sapply(args, is_valid))) {
    stop("arguments must be ModelFrames with identical response variables")
  }

  data <- NULL
  for (i in seq(args)) {
    data <- combine_dataframes(as.data.frame(args[[i]]), data)
  }

  new("SelectedModelFrame", ModelFrame(data),
      terms = lapply(args, terms),
      params = list(control = getMLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff))

}
