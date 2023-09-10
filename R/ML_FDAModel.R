#' Flexible and Penalized Discriminant Analysis Models
#'
#' Performs flexible discriminant analysis.
#'
#' @rdname FDAModel
#'
#' @param theta optional matrix of class scores, typically with number of
#'   columns less than one minus the number of classes.
#' @param dimension dimension of the discriminant subspace, less than the number
#'   of classes, to use for prediction.
#' @param eps numeric threshold for small singular values for excluding
#'   discriminant variables.
#' @param method regression function used in optimal scaling.  The default of
#'   linear regression is provided by \code{\link[mda]{polyreg}} from the
#'   \pkg{mda} package.  For penalized discriminant analysis,
#'   \code{\link[mda]{gen.ridge}} is appropriate.  Other possibilities are
#'   \code{\link[mda]{mars}} for multivariate adaptive regression splines and
#'   \code{\link[mda]{bruto}} for adaptive backfitting of additive splines.  Use
#'   the \code{\link[=quote]{.}} operator to quote specified functions.
#' @param ... additional arguments to \code{method} for \code{FDAModel} and to
#'   \code{FDAModel} for \code{PDAModel}.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \itemize{
#'       \item FDAModel: \code{nprune}, \code{degree}*
#'       \item PDAModel: \code{lambda}
#'     }
#'   }
#' }
#' * excluded from grids by default
#'
#' The \code{\link{predict}} function for this model additionally accepts the
#' following argument.
#' \describe{
#'   \item{\code{prior}}{prior class membership probabilities for prediction
#'     data if different from the training set.}
#' }
#'
#' Default argument values and further model details can be found in the source
#' See Also links below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[mda]{fda}}, \code{\link[mda]{predict.fda}},
#' \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package mda to run
#'
#' fit(Species ~ ., data = iris, model = FDAModel)
#' }
#'
FDAModel <- function(
  theta = matrix(NA, 0, 0), dimension = integer(), eps = .Machine$double.eps,
  method = .(mda::polyreg), ...
) {

  MLModel(

    name = "FDAModel",
    label = "Flexible Discriminant Analysis",
    packages = "mda",
    response_types = "factor",
    weights = TRUE,
    predictor_encoding = "model.matrix",
    na.rm = TRUE,
    params = new_params(environment(), ...),

    gridinfo = new_gridinfo(
      param = c("nprune", "degree"),
      get_values = c(
        function(n, data, ...) {
          model_fit <- fit(data, model = EarthModel(pmethod = "none"))
          max_terms <- min(2 + 0.75 * nrow(model_fit$dirs), 200)
          round_int(seq(2, max_terms, length = n))
        },
        function(n, ...) head(1:2, n)
      ),
      default = c(TRUE, FALSE)
    ),

    fit = function(formula, data, weights, ...) {
      mda::fda(
        formula, data = as.data.frame(formula, data = data), weights = weights,
        ...
      )
    },

    predict = function(object, newdata, prior = object$prior, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "posterior", prior = prior)
    }

  )

}

MLModelFunction(FDAModel) <- NULL


#' @rdname FDAModel
#'
#' @param lambda shrinkage penalty coefficient.
#' @param df alternative specification of \code{lambda} in terms of equivalent
#'   degrees of freedom.
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package mda to run
#'
#' fit(Species ~ ., data = iris, model = PDAModel)
#' }
#'
PDAModel <- function(lambda = 1, df = numeric(), ...) {
  params <- new_params(environment(), ...)
  params$method <- .(mda::gen.ridge)
  model <- do.call(FDAModel, params, quote = TRUE)
  model@name <- "PDAModel"
  model@label <- "Penalized Discriminant Analysis"
  model@gridinfo <- new_gridinfo(
    param = "lambda",
    get_values = c(
      function(n, ...) c(0, 10^seq_inner(-5, 1, n - 1))
    )
  )
  model
}

MLModelFunction(PDAModel) <- NULL
