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
#'   \item{Response types:}{\code{factor}, \code{numeric}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
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
    na.rm = TRUE,
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = "ncomp",
      get_values = c(
        function(n, data, ...) {
          seq_len(min(n, nrow(data), nvars(data, PLSModel) - 1))
        }
      )
    ),

    fit = function(formula, data, weights, ...) {
      y <- response(data)
      data <- as.data.frame(formula, data = data)
      if (is.factor(y)) {
        mm <- model.matrix(~ y - 1)
        colnames(mm) <- levels(y)
        data[[response(formula)]] <- mm
      }
      pls::plsr(formula, data = data, na.action = na.pass, ...)
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(
        object, newdata = newdata, ncomp = object$ncomp, type = "response"
      )
    },

    varimp = function(object, ...) {
      beta <- coef(object, comps = seq_len(object$ncomp))
      perf <- quote(MSEP.mvr(x)$val[1, , , drop = FALSE]) %>%
        eval(list(x = object), asNamespace("pls"))
      vi <- map("num", function(i) {
        drop(as.matrix(abs(beta[, i, ])) %*% proportions(-diff(perf[, i, ])))
      }, seq_len(ncol(beta)))
      dimnames(vi) <- dimnames(beta)[1:2]
      if (ncol(vi) <= 2) vi <- vi[, 1]
      structure(vi, metric = "coefficient")
    }

  )

}

MLModelFunction(PLSModel) <- NULL
