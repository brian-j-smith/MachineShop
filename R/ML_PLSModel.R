#' Partial Least Squares Model
#'
#' Function to perform partial least squares regression.
#'
#' @param ncomp number of components to include in the model.
#' @param scale logical indicating whether to scale the predictors by the
#'   sample standard deviation.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{ncomp}
#'   }
#' }
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[pls]{mvr}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package pls to run
#'
#' fit(sale_amount ~ ., data = ICHomes, model = PLSModel)
#' }
#'
PLSModel <- function(ncomp = 1, scale = FALSE) {

  MLModel(
    name = "PLSModel",
    label = "Partial Least Squares",
    packages = "pls",
    response_types = c("factor", "numeric"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    gridinfo = new_gridinfo(
      param = "ncomp",
      values = c(
        function(n, data, ...) 1:min(nrow(data), nvars(data, PLSModel) - 1, n)
      )
    ),
    fit = function(formula, data, weights, ...) {
      throw(check_equal_weights(weights))
      y <- response(data)
      data <- as.data.frame(data)
      if (is.factor(y)) {
        mm <- model.matrix(~ y - 1)
        colnames(mm) <- levels(y)
        data[[response(formula)]] <- mm

      }
      pls::plsr(formula, data = data, ...)
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, ncomp = object$ncomp,
              type = "response")
    },
    varimp = function(object, ...) {
      beta <- coef(object, comps = 1:object$ncomp)
      perf <- quote(MSEP.mvr(x)$val[1, , , drop = FALSE]) %>%
        eval(list(x = object), asNamespace("pls"))
      vi <- map_num(function(i) {
        drop(as.matrix(abs(beta[, i, ])) %*% prop.table(-diff(perf[, i, ])))
      }, 1:dim(beta)[2])
      dimnames(vi) <- dimnames(beta)[1:2]
      if (ncol(vi) <= 2) vi <- vi[, 1]
      vi
    }
  )

}

MLModelFunction(PLSModel) <- NULL
