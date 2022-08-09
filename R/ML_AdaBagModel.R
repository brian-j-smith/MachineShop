#' Bagging with Classification Trees
#'
#' Fits the Bagging algorithm proposed by Breiman in 1996 using classification
#' trees as single classifiers.
#'
#' @param mfinal number of trees to use.
#' @param minsplit minimum number of observations that must exist in a node in
#'   order for a split to be attempted.
#' @param minbucket minimum number of observations in any terminal node.
#' @param cp complexity parameter.
#' @param maxcompete number of competitor splits retained in the output.
#' @param maxsurrogate number of surrogate splits retained in the output.
#' @param usesurrogate how to use surrogates in the splitting process.
#' @param xval number of cross-validations.
#' @param surrogatestyle controls the selection of a best surrogate.
#' @param maxdepth maximum depth of any node of the final tree, with the root
#'   node counted as depth 0.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{mfinal}, \code{maxdepth}
#'   }
#' }
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[adabag]{bagging}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package adabag to run
#'
#' fit(Species ~ ., data = iris, model = AdaBagModel(mfinal = 5))
#' }
#'
AdaBagModel <- function(
  mfinal = 100, minsplit = 20, minbucket = round(minsplit/3), cp = 0.01,
  maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
  surrogatestyle = 0, maxdepth = 30
) {

  MLModel(

    name = "AdaBagModel",
    label = "Bagging with Classification Trees",
    packages = "adabag",
    response_types = "factor",
    predictor_encoding = "model.frame",
    na.rm = FALSE,
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("mfinal", "maxdepth"),
      get_values = c(
        function(n, ...) round_int(seq_range(0, 25, c(1, 200), n + 1)),
        function(n, ...) seq_len(min(n, 30))
      )
    ),

    fit = function(formula, data, weights, mfinal, ...) {
      adabag::bagging(
        formula, data = as.data.frame(formula, data), mfinal = mfinal,
        control = list(...)
      )
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata)$prob
    },

    varimp = function(object, ...) {
      object$importance
    }

  )

}

MLModelFunction(AdaBagModel) <- NULL
