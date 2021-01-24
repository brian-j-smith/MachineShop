#' Weighted k-Nearest Neighbor Model
#'
#' Fit a k-nearest neighbor model for which the k nearest training set vectors
#' (according to Minkowski distance) are found for each row of the test set, and
#' prediction is done via the maximum of summed kernel densities.
#'
#' @param k numer of neigbors considered.
#' @param distance Minkowski distance parameter.
#' @param scale logical indicating whether to scale predictors to have equal
#'   standard deviations.
#' @param kernel kernel to use.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordinal}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{k}, \code{distance}*, \code{kernel}*
#'   }
#' }
#' * included only in randomly sampled grid points
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[kknn]{kknn}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package kknn to run
#'
#' fit(Species ~ ., data = iris, model = KNNModel)
#' }
#'
KNNModel <- function(
  k = 7, distance = 2, scale = TRUE,
  kernel = c("optimal", "biweight", "cos", "epanechnikov", "gaussian", "inv",
             "rank", "rectangular", "triangular", "triweight")
) {

  kernel <- match.arg(kernel)

  MLModel(
    name = "KNNModel",
    label = "K-Nearest Neighbors Model",
    packages = "kknn",
    response_types = c("factor", "numeric", "ordered"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    grid = function(x, length, random, ...) {
      params <- list(
        k = round(seq_range(0, 5, c(1, nrow(x) / 3), length + 1))
      )
      if (random) {
        params$distance <- seq_inner(0, 4, length = length)
        kernel <- c("optimal", "biweight", "cos", "epanechnikov", "gaussian",
                    "inv", "rank", "rectangular", "triangular", "triweight")
        params$kernel <- head(sample(kernel), length)
      }
      params
    },
    fit = function(formula, data, weights, ...) {
      assert_equal_weights(weights)
      list(formula = formula, train = as.data.frame(data), ...)
    },
    predict = function(object, newdata, ...) {
      attach_objects(list(
        contr.dummy = kknn::contr.dummy,
        contr.ordinal = kknn::contr.ordinal,
        contr.metrics = kknn::contr.metric
      ), name = "kknn_exports")

      object$test <- as.data.frame(newdata)
      pred <- do.call(kknn::kknn, object)
      if (pred$response == "continuous") pred$fitted.values else pred$prob
    }
  )

}

MLModelFunction(KNNModel) <- NULL
