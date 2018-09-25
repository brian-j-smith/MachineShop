#' Support Vector Machine Models
#' 
#' Fits the well known C-svc, nu-svc, (classification) one-class-svc (novelty)
#' eps-svr, nu-svr (regression) formulations along with native multi-class
#' classification formulations and the bound-constraint SVM formulations.
#' 
#' @rdname SVMModel
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
#' Arguments \code{kernel} and \code{kpar} are automatically set by the
#' kernel-specific constructor functions.
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
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
    fit = function(formula, data, weights, ...) {
      if (!all(weights == 1)) warning("weights are unsupported and will be ignored")
      environment(formula) <- environment()
      kernlab::ksvm(formula, data = data, prob.model = TRUE, ...) %>%
        asMLModelFit("SVMFit", SVMModel(...))
    },
    predict = function(object, newdata, ...) {
      kernlab::predict(unMLModelFit(object), newdata = newdata,
                       type = ifelse(is.factor(response(object)),
                                     "probabilities", "response"))
    },
    response = function(object, ...) {
      if (is.character(object@lev)) {
        factor(object@ymatrix, levels = 1:object@nclass, labels = object@lev)
      } else {
        object@ymatrix
      }
    }
  )
}


#' @rdname SVMModel
#' 
#' @param sigma inverse kernel width used by the ANOVA, Bessel, and Laplacian
#' kernels.
#' @param degree degree of the ANOVA, Bessel, and polynomial kernel functions.
#' @param ... arguments to be passed to \code{SVMModel}.
#' 
SVMANOVAModel <- function(sigma = NULL, degree = NULL, ...) {
  .SVMModel("anovadot", environment(), ...)
}


#' @rdname SVMModel
#' 
#' @param order order of the Bessel function to be used as a kernel.
#' 
SVMBesselModel <- function(sigma = NULL, order = NULL, degree = NULL, ...) {
  .SVMModel("besseldot", environment(), ...)
}


#' @rdname SVMModel
#' 
SVMLaplaceModel <- function(sigma = NULL, ...) {
  .SVMModel("laplacedot", environment(), ...)
}


#' @rdname SVMModel
#' 
SVMLinearModel <- function(...) {
  .SVMModel("vanilladot", environment(), ...)
}


#' @rdname SVMModel
#' 
#' @param scale scaling parameter of the polynomial and hyperbolic tangent
#' kernels as a convenient way of normalizing patterns without the need to
#' modify the data itself.
#' @param offset offset used in polynomial and hyperbolic tangent kernels.
#' 
SVMPolyModel <- function(degree = NULL, scale = NULL, offset = NULL, ...) {
  .SVMModel("polydot", environment(), ...)
}


#' @rdname SVMModel
#' 
SVMRadialModel <- function(sigma = NULL, ...) {
  .SVMModel("rbfdot", environment(), ...)
}


#' @rdname SVMModel
#' 
SVMSplineModel <- function(...) {
  .SVMModel("splinedot", environment(), ...)
}


#' @rdname SVMModel
#' 
SVMTanhModel <- function(scale = NULL, offset = NULL, ...) {
  .SVMModel("tanhdot", environment(), ...)
}


.SVMModel <- function(kernel, envir, ...) {
  args <- list(...)
  args$kernel <- kernel
  kpar <- params(envir)
  if (length(kpar)) args$kpar <- as.call(c(quote(list), kpar))
  do.call(SVMModel, args, quote = TRUE)
}
