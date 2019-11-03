#' Gradient Boosting with Linear Models
#' 
#' Gradient boosting for optimizing arbitrary loss functions where
#' component-wise linear models are utilized as base-learners.
#' 
#' @param family optional \code{\link[mboost]{Family}} object.  Set
#'   automatically according to the class type of the response variable.
#' @param mstop number of initial boosting iterations.
#' @param nu step size or shrinkage parameter between 0 and 1.
#' @param risk method to use in computing the empirical risk for each boosting
#'   iteration.
#' @param stopintern logical inidicating whether the boosting algorithm stops
#'   internally when the out-of-bag risk increases at a subsequent iteration.
#' @param trace logical indicating whether status information is printed during
#'   the fitting process.
#' 
#' @details
#' \describe{
#'   \item{Response Types:}{\code{binary}, \code{numeric}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{mstop}
#'   }
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[mboost]{glmboost}}, \code{\link[mboost]{Family}},
#' \code{\link{fit}}, \code{\link{resample}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(type ~ ., data = Pima.tr, model = GLMBoostModel)
#'
GLMBoostModel <- function(family = NULL, mstop = 100, nu = 0.1,
                          risk = c("inbag", "oobag", "none"),
                          stopintern = FALSE, trace = FALSE) {
  
  args <- params(environment())
  is_main <- names(args) %in% "family"
  params <- args[is_main]
  params$control <- as.call(c(.(mboost::boost_control), args[!is_main]))

  MLModel(
    name = "GLMBoostModel",
    label = "Gradient Boosting with Linear Models",
    packages = "mboost",
    response_types = c("binary", "numeric", "Surv"),
    predictor_encoding = "terms",
    params = params,
    grid = function(x, length, ...) {
      list(
        mstop = round(seq_range(0, 50, c(1, 1000), length + 1))
      )
    },
    fit = function(formula, data, weights, family = NULL, ...) {
      if (is.null(family)) {
        family <- switch_class(response(data),
                               "factor" = mboost::Binomial(),
                               "numeric" = mboost::Gaussian(),
                               "Surv" = mboost::CoxPH())
      }
      eval_fit(data,
               formula = mboost::glmboost(formula, data = as.data.frame(data),
                                          na.action = na.pass,
                                          weights = weights, family = family,
                                          ...),
               matrix = mboost::glmboost(x, y, weights = weights,
                                         family = family, ...))
    },
    predict = function(object, newdata, times, ...) {
      newdata <- as.data.frame(newdata)
      if (object$family@name == "Cox Partial Likelihood") {
        y <- object$response
        lp <- drop(predict(object, type = "link"))
        new_lp <- drop(predict(object, newdata = newdata, type = "link"))
        predict(y, lp, times, new_lp, ...)
      } else {
        predict(object, newdata = newdata, type = "response")
      }
    },
    varimp = function(object, ...) {
      structure(mboost::varimp(object), class = "numeric")
    }
  )
  
}

MLModelFunction(GLMBoostModel) <- NULL
