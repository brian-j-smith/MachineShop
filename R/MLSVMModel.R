#' Support Vector Machine Model
#' 
#' Fits the well known C-svc, nu-svc, (classification) one-class-svc (novelty)
#' eps-svr, nu-svr (regression) formulations along with native multi-class
#' classification formulations and the bound-constraint SVM formulations.
#' 
#' @param scaled logical vector indicating the variables to be scaled.
#' @param type type of support vector machine.
#' @param kernel kernel function used in training and predicting.
#' @param kpar list of hyper-parameters (kernel parameters).
#' @param C cost of constraints violation defined as the regularization term in
#' the Lagrange formulation.
#' @param nu parameter needed for nu-svc, one-svc, and nu-svr.
#' @param epsilon parameter in the insensitive-loss function used for eps-svr,
#' nu-svr and eps-bsvm.
#' @param cache cache memory in MB.
#' @param tol tolerance of termination criterion.
#' @param shrinking whether to use the shrinking-heuristics.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[kernlab]{ksvm}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#'
SVMModel <- function(scaled = NULL, type = NULL, kernel = NULL, kpar = NULL,
                     C = NULL, nu = NULL, epsilon = NULL, cache = NULL,
                     tol = NULL, shrinking = NULL) {
  MLModel(
    name = "SVMModel",
    packages = "kernlab",
    types = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = NULL, ...) {
      if(!is.null(weights)) warning("weights are unsupported and will be ignored")
      environment(formula) <- environment()
      kernlab::ksvm(formula, data = data, prob.model = TRUE, ...) %>%
        asMLModelFit("SVMFit", SVMModel(...))
    },
    predict = function(object, newdata, ...) {
      kernlab::predict(asParentFit(object), newdata = newdata,
                       type = ifelse(is.factor(response(object)),
                                     "probabilities", "response"))
    },
    response = function(object, ...) {
      if(is.character(object@lev)) {
        factor(object@ymatrix, levels = 1:object@nclass, labels = object@lev)
      } else {
        object@ymatrix
      }
    }
  )
}
