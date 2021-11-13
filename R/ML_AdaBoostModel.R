#' Boosting with Classification Trees
#'
#' Fits the AdaBoost.M1 (Freund and Schapire, 1996) and SAMME (Zhu et al., 2009)
#' algorithms using classification trees as single classifiers.
#'
#' @param boos if \code{TRUE}, then bootstrap samples are drawn from the
#'   training set using the observation weights at each iteration.  If
#'   \code{FALSE}, then all observations are used with their weights.
#' @param mfinal number of iterations for which boosting is run.
#' @param coeflearn learning algorithm.
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
#'   \item{Response Types:}{\code{factor}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{mfinal}, \code{maxdepth}, \code{coeflearn}*
#'   }
#' }
#' * excluded from grids by default
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[adabag]{boosting}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package adabag to run
#'
#' fit(Species ~ ., data = iris, model = AdaBoostModel(mfinal = 5))
#' }
#'
AdaBoostModel <- function(
  boos = TRUE, mfinal = 100, coeflearn = c("Breiman", "Freund", "Zhu"),
  minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, maxcompete = 4,
  maxsurrogate = 5, usesurrogate = 2, xval = 10, surrogatestyle = 0,
  maxdepth = 30
) {

  coeflearn <- match.arg(coeflearn)

  MLModel(
    name = "AdaBoostModel",
    label = "Boosting with Classification Trees",
    packages = "adabag",
    response_types = "factor",
    predictor_encoding = "model.frame",
    params = new_params(environment()),
    gridinfo = new_gridinfo(
      param = c("mfinal", "maxdepth", "coeflearn"),
      get_values = c(
        function(n, ...) round(seq_range(0, 25, c(1, 200), n + 1)),
        function(n, ...) seq_len(min(n, 30)),
        function(n, ...) head(c("Breiman", "Freund", "Zhu"), n)
      ),
      default = c(TRUE, TRUE, FALSE)
    ),
    fit = function(formula, data, weights, boos, mfinal, coeflearn, ...) {
      adabag::boosting(
        formula, data = as.data.frame(data), boos = boos, mfinal = mfinal,
        coeflearn = coeflearn, control = list(...)
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

MLModelFunction(AdaBoostModel) <- NULL
