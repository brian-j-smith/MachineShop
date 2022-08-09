#' Linear Discriminant Analysis Model
#'
#' Performs linear discriminant analysis.
#'
#' @param prior prior probabilities of class membership if specified or the
#'   class proportions in the training set otherwise.
#' @param tol tolerance for the determination of singular matrices.
#' @param method type of mean and variance estimator.
#' @param nu degrees of freedom for \code{method = "t"}.
#' @param dimen dimension of the space to use for prediction.
#' @param use type of parameter estimation to use for prediction.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameter:}{
#'     \code{dimen}
#'   }
#' }
#'
#' The \code{\link{predict}} function for this model additionally accepts the
#' following argument.
#' \describe{
#'   \item{\code{prior}}{prior class membership probabilities for prediction
#'     data if different from the training set.}
#' }
#'
#' Default values and further model details can be found in the source links
#' below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}},
#' \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(Species ~ ., data = iris, model = LDAModel)
#'
LDAModel <- function(
  prior = numeric(), tol = 1e-4, method = c("moment", "mle", "mve", "t"),
  nu = 5, dimen = integer(), use = c("plug-in", "debiased", "predictive")
) {

  method <- match.arg(method)
  use <- match.arg(use)

  MLModel(

    name = "LDAModel",
    label = "Linear Discriminant Analysis",
    packages = "MASS",
    response_types = "factor",
    predictor_encoding = "model.matrix",
    na.rm = TRUE,
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = "dimen",
      get_values = c(
        function(n, data, ...) {
          seq_len(min(n, nlevels(response(data)) - 1, nvars(data, LDAModel)))
        }
      )
    ),

    fit = function(formula, data, weights, dimen, use, ...) {
      res <- MASS::lda(
        formula, data = as.data.frame(formula, data), na.action = na.pass, ...
      )
      attr(res, ".MachineShop") <- list(
        dimen = if (missing(dimen)) length(res$svd) else dimen,
        use = use
      )
      res
    },

    predict = function(
      object, newdata, prior = object$prior, .MachineShop, ...
    ) {
      newdata <- as.data.frame(newdata)
      predict(
        object, newdata = newdata, prior = prior, dimen = .MachineShop$dimen,
        method = .MachineShop$use
      )$posterior
    }

  )

}

MLModelFunction(LDAModel) <- NULL
