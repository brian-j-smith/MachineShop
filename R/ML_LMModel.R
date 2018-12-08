#' Linear Models
#'
#' Fits linear models.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{matrix}, \code{numeric}}
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
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = LMModel())
#'
LMModel <- function() {
  
  MLModel(
    name = "LMModel",
    packages = "stats",
    types = c("factor", "matrix", "numeric"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      y <- response(formula, data)
      if (is.factor(y)) {
        y_name <- response(terms(formula))
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
      predict(object, newdata = newdata)
    },
    varimp = function(object, ...) varimp_pchisq(object)
  )
  
}
