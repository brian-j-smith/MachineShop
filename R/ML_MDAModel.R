#' Mixture Discriminant Analysis Model
#'
#' Performs mixture discriminant analysis.
#'
#' @param subclasses numeric value or vector of subclasses per class.
#' @param sub.df effective degrees of freedom of the centroids per class if
#'   subclass centroid shrinkage is performed.
#' @param tot.df specification of the total degrees of freedom as an alternative
#'   to \code{sub.df}.
#' @param dimension dimension of the discriminant subspace to use for
#'   prediction.
#' @param eps numeric threshold for automatically truncating the dimension.
#' @param iter limit on the total number of iterations.
#' @param method regression function used in optimal scaling.  The default of
#'   linear regression is provided by \code{\link[mda]{polyreg}} from the
#'   \pkg{mda} package.  For penalized mixture discriminant models,
#'   \code{\link[mda]{gen.ridge}} is appropriate.  Other possibilities are
#'   \code{\link[mda]{mars}} for multivariate adaptive regression splines and
#'   \code{\link[mda]{bruto}} for adaptive backfitting of additive splines.  Use
#'   the \code{\link[MachineShop:dot-]{.}} operator to quote specified
#'   functions.
#' @param trace logical indicating whether iteration information is printed.
#' @param ... additional arguments to \code{mda.start} and \code{method}.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{subclasses}
#'   }
#' }
#'
#' The \code{\link{predict}} function for this model additionally accepts the
#' following argument.
#' \describe{
#'   \item{\code{prior}}{prior class membership probabilities for prediction data
#'     if different from the training set.}
#' }
#'
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[mda]{mda}}, \code{\link[mda]{predict.mda}},
#' \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(Species ~ ., data = iris, model = MDAModel)
#'
MDAModel <- function(subclasses = 3, sub.df = NULL, tot.df = NULL,
                     dimension = sum(subclasses) - 1, eps = .Machine$double.eps,
                     iter = 5, method = .(mda::polyreg), trace = FALSE, ...) {

  MLModel(
    name = "MDAModel",
    label = "Mixture Discriminant Analysis",
    packages = "mda",
    response_types = "factor",
    predictor_encoding = "model.matrix",
    params = params(environment()),
    grid = function(x, length, ...) {
      list(
        subclasses = 1:min(length, 10) + 1
      )
    },
    fit = function(formula, data, weights, ...) {
      assert_equal_weights(weights)
      mda::mda(formula, data = as.data.frame(data), ...)
    },
    predict = function(object, newdata, prior = object$prior, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "posterior", prior = prior)
    }
  )

}

MLModelFunction(MDAModel) <- NULL
