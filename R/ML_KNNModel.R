#' Weighted k-Nearest Neighbor Model
#' 
#' Fit a k-nearest neighbor model for which the k nearest training set vectors
#' (according to Minkowski distance) are found for each row of the test set, and
#' prediction is done via the maximum of summed kernel densities.
#'
#' @param k numer of neigbors considered.
#' @param distance Minkowski distance parameter.
#' @param scale logical indicating whether to scale predictors to have equal
#' standard deviations.
#' @param kernel kernel to use.
#'
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{ordinal}}
#' }
#' 
#' Further model details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[kknn]{kknn}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = KNNModel())
#'
KNNModel <- function(k = 7, distance = 2, scale = TRUE,
                     kernel = c("optimal", "biweight", "cos", "epanechnikov",
                                "gaussian", "inv", "rank", "rectangular",
                                "triangular", "triweight")) {
  kernel <- match.arg(kernel)
  
  MLModel(
    name = "KNNModel",
    packages = "kknn",
    types = c("factor", "numeric", "ordered"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      assert_equal_weights(weights)
      list(formula = formula, ...)
    },
    predict = function(object, newdata, ...) {
      args <- unMLModelFit(object)
      args$train <- preprocess(fitbit(object, "x"))
      args$test <- newdata
      pred <- do.call(kknn::kknn, args)
      if (pred$response == "continuous") pred$fitted.values else pred$prob
    },
    varimp = function(object, ...) {
      warning("variable importance values undefined for KNNModel")
      varnames <- labels(terms(object$formula))
      structure(rep(NA_integer_, length(varnames)), names = varnames)
    }
  )
}
