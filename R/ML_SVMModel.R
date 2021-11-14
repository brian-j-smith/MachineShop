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
#'   the Lagrange formulation.
#' @param nu parameter needed for nu-svc, one-svc, and nu-svr.
#' @param epsilon parameter in the insensitive-loss function used for eps-svr,
#'   nu-svr and eps-bsvm.
#' @param cache cache memory in MB.
#' @param tol tolerance of termination criterion.
#' @param shrinking whether to use the shrinking-heuristics.
#' @param sigma inverse kernel width used by the ANOVA, Bessel, and Laplacian
#'   kernels.
#' @param degree degree of the ANOVA, Bessel, and polynomial kernel functions.
#' @param order order of the Bessel function to be used as a kernel.
#' @param scale scaling parameter of the polynomial and hyperbolic tangent
#'   kernels as a convenient way of normalizing patterns without the need to
#'   modify the data itself.
#' @param offset offset used in polynomial and hyperbolic tangent kernels.
#' @param ... arguments passed to \code{SVMModel} from the other constructors.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters}{
#'     \itemize{
#'       \item SVMANOVAModel: \code{C}, \code{degree}
#'       \item SVMBesselModel: \code{C}, \code{order}, \code{degree}
#'       \item SVMLaplaceModel: \code{C}, \code{sigma}
#'       \item SVMLinearModel: \code{C}
#'       \item SVMPolyModel: \code{C}, \code{degree}, \code{scale}
#'       \item SVMRadialModel: \code{C}, \code{sigma}
#'     }
#'   }
#' }
#'
#' Arguments \code{kernel} and \code{kpar} are automatically set by the
#' kernel-specific constructor functions.
#' Default values and further model details can be found in the source link
#' below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[kernlab]{ksvm}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = SVMRadialModel)
#'
SVMModel <- function(
  scaled = TRUE, type = character(),
  kernel = c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot",
             "besseldot", "anovadot", "splinedot"),
  kpar = "automatic", C = 1, nu = 0.2, epsilon = 0.1, cache = 40, tol = 0.001,
  shrinking = TRUE
) {

  kernel <- match.arg(kernel)

  MLModel(

    name = "SVMModel",
    label = "Support Vector Machines",
    packages = "kernlab",
    response_types = c("factor", "numeric"),
    predictor_encoding = "model.matrix",
    params = new_params(environment()),

    fit = function(formula, data, weights, ...) {
      eval_fit(
        data,
        formula = kernlab::ksvm(formula, data = data, prob.model = TRUE, ...),
        matrix = kernlab::ksvm(x, y, prob.model = TRUE, ...)
      )
    },

    predict = function(object, newdata, model, ...) {
      newdata <- as.data.frame(newdata)
      kernlab::predict(
        object, newdata = newdata,
        type = if (is.factor(response(model))) "probabilities" else "response"
      )
    }

  )

}

MLModelFunction(SVMModel) <- NULL


#' @rdname SVMModel
#'
SVMANOVAModel <- function(sigma = 1, degree = 1, ...) {
  .SVMModel(name = "SVMANOVAModel", label = "Support Vector Machines (ANOVA)",
            model = "anovadot", envir = environment(), ...)
}

MLModelFunction(SVMANOVAModel) <- NULL


#' @rdname SVMModel
#'
SVMBesselModel <- function(sigma = 1, order = 1, degree = 1, ...) {
  .SVMModel(name = "SVMBesselModel", label = "Support Vector Machines (Bessel)",
            model = "besseldot", envir = environment(), ...)
}

MLModelFunction(SVMBesselModel) <- NULL


#' @rdname SVMModel
#'
SVMLaplaceModel <- function(sigma = numeric(), ...) {
  .SVMModel(name = "SVMLaplaceModel", label = "Support Vector Machines (Laplace)",
            model = "laplacedot", envir = environment(), ...)
}

MLModelFunction(SVMLaplaceModel) <- NULL


#' @rdname SVMModel
#'
SVMLinearModel <- function(...) {
  .SVMModel(name = "SVMLinearModel", label = "Support Vector Machines (Linear)",
            model = "vanilladot", envir = environment(), ...)
}

MLModelFunction(SVMLinearModel) <- NULL


#' @rdname SVMModel
#'
SVMPolyModel <- function(degree = 1, scale = 1, offset = 1, ...) {
  .SVMModel(name = "SVMPolyModel", label = "Support Vector Machines (Poly)",
            model = "polydot", envir = environment(), ...)
}

MLModelFunction(SVMPolyModel) <- NULL


#' @rdname SVMModel
#'
SVMRadialModel <- function(sigma = numeric(), ...) {
  .SVMModel(name = "SVMRadialModel", label = "Support Vector Machines (Radial)",
            model = "rbfdot", envir = environment(), ...)
}

MLModelFunction(SVMRadialModel) <- NULL


#' @rdname SVMModel
#'
SVMSplineModel <- function(...) {
  .SVMModel(name = "SVMSplineModel", label = "Support Vector Machines (Spline)",
            model = "splinedot", envir = environment(), ...)
}

MLModelFunction(SVMSplineModel) <- NULL


#' @rdname SVMModel
#'
SVMTanhModel <- function(scale = 1, offset = 1, ...) {
  .SVMModel(name = "SVMTanhModel", label = "Support Vector Machines (Tanh)",
            model = "tanhdot", envir = environment(), ...)
}

MLModelFunction(SVMTanhModel) <- NULL


.SVMModel <- function(name, label, model, envir, ...) {
  params <- list(...)
  params$kernel <- model
  kpar <- new_params(envir)
  params$kpar <- if (
    params$kernel %in% c("laplacedot", "rbfdot") && is_empty(kpar)
  ) {
    "automatic"
  } else {
    as.call(c(.(list), kpar))
  }
  model <- do.call(SVMModel, params, quote = TRUE)
  model@name <- name
  model@label <- label

  scaled <- model@params$scaled
  if (!is.logical(scaled)) scaled <- TRUE

  gridinfo <- new_gridinfo(
    param = c("C", "degree", "order", "scale", "sigma"),
    get_values = c(
      function(n, ...) 2^seq_range(-4, 2, c(-4, 10), n),
      function(n, ...) seq_len(min(n, 3)),
      function(n, ...) seq_len(min(n, 3)),
      function(n, ...) 10^seq_range(-4, 2, c(-4, log10(2)), n),
      function(n, data, ...) {
        sigmas <- kernlab::sigest(model.matrix(data, intercept = FALSE),
                                  scaled = scaled)
        exp(seq(log(min(sigmas)), log(max(sigmas)), length = n))
      }
    )
  )
  grid_params <- switch(params$kernel,
    "anovadot" = c("C", "degree"),
    "besseldot" = c("C", "order", "degree"),
    "laplacedot" = c("C", "sigma"),
    "polydot" = c("C", "degree", "scale"),
    "rbfdot" = c("C", "sigma"),
    "vanilladot" = "C"
  )
  model@gridinfo <- gridinfo[gridinfo$param %in% grid_params, ]

  model
}
