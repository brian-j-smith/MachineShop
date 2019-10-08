#' Linear Models
#'
#' Fits linear models.
#' 
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{matrix}, \code{numeric}}
#' }
#' 
#' Further model details can be found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[stats]{lm}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = LMModel)
#'
LMModel <- function() {
  
  MLModel(
    name = "LMModel",
    label = "Linear Model",
    packages = "stats",
    response_types = c("factor", "matrix", "numeric"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    fit = function(formula, data, weights, ...) {
      y <- response(data)
      data <- as.data.frame(data)
      if (is.factor(y)) {
        y_name <- deparse(response(formula))
        formula[[2]] <- as.symbol(y_name)
        if (nlevels(y) == 2) {
          y <- y == levels(y)[2]
          data[[y_name]] <- y
        } else {
          mm <- model.matrix(~ y - 1)
          colnames(mm) <- levels(y)
          data[[y_name]] <- mm
        }
      }
      if (is.numeric(y)) {
        stats::lm(formula, data = data, weights = weights, ...)
      } else {
        assert_equal_weights(weights)
        stats::lm(formula, data = data, ...)
      }
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata)
    },
    varimp = function(object, ...) varimp_wald(object)
  )
  
}
