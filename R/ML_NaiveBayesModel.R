#' Naive Bayes Classifier Model
#'
#' Computes the conditional a-posterior probabilities of a categorical class
#' variable given independent predictor variables using Bayes rule.
#'
#' @param laplace positive numeric controlling Laplace smoothing.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}}
#' }
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[e1071]{naiveBayes}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package e1071 to run
#'
#' fit(Species ~ ., data = iris, model = NaiveBayesModel)
#' }
#'
NaiveBayesModel <- function(laplace = 0) {

  MLModel(
    name = "NaiveBayesModel",
    label = "Naive Bayes Classifier",
    packages = "e1071",
    response_types = "factor",
    predictor_encoding = "model.frame",
    params = new_params(environment()),
    fit = function(formula, data, weights, ...) {
      eval_fit(data,
               formula = e1071::naiveBayes(formula, data = as.data.frame(data),
                                           ...),
               matrix = e1071::naiveBayes(x, y, ...))
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "raw")
    }
  )

}

MLModelFunction(NaiveBayesModel) <- NULL
