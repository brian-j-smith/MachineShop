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
      y_name <- response(terms(formula))
      if (is_response(y, "binary")) {
        y <- y == levels(y)[2]
        data[[y_name]] <- y
        formula[[2]] <- as.symbol(y_name)
      } else if (is(y, "factor")) {
        mm <- model.matrix(~ y - 1)
        colnames(mm) <- levels(y)
        data[[y_name]] <- mm
        formula[[2]] <- as.symbol(y_name)
      }
      args <- list(formula = formula, data = data, ...)
      if (is(y, "numeric")) {
        args$weights <- weights
      } else {
        assert_equal_weights(weights)
      }
      do.call(stats::lm, args)
    },
    predict = function(object, newdata, ...) {
      predict(object, newdata = newdata)
    },
    varimp = function(object, ...) varimp_pchisq(object)
  )
}
