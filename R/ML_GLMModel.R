#' Generalized Linear Model
#'
#' Fits generalized linear models, specified by giving a symbolic description of
#' the linear predictor and a description of the error distribution.
#'
#' @rdname GLMModel
#'
#' @param family optional error distribution and link function to be used in the
#'   model.  Set automatically according to the class type of the response
#'   variable.
#' @param quasi logical indicator for over-dispersion of binomial and Poisson
#'   families; i.e., dispersion parameters not fixed at one.
#' @param ... arguments passed to \code{\link[stats]{glm.control}}.
#'
#' @details
#' \describe{
#'   \item{\code{GLMModel} Response Types:}{\code{BinomialVariate},
#'     \code{factor}, \code{matrix}, \code{NegBinomialVariate},
#'     \code{numeric}, \code{PoissonVariate}}
#'   \item{\code{GLMStepAICModel} Response Types:}{\code{binary factor},
#'     \code{BinomialVariate}, \code{NegBinomialVariate}, \code{numeric},
#'     \code{PoissonVariate}}
#' }
#'
#' Default values and further model details can be found in the source links
#' below.
#'
#' In calls to \code{\link{varimp}} for \code{GLMModel} and
#' \code{GLMStepAICModel}, numeric argument \code{base} may be specified for the
#' (negative) logarithmic transformation of p-values [defaul: \code{exp(1)}].
#' Transformed p-values are automatically scaled in the calculation of variable
#' importance to range from 0 to 100.  To obtain unscaled importance values, set
#' \code{scale = FALSE}.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[stats]{glm}}, \code{\link[stats]{glm.control}},
#' \code{\link[MASS]{stepAIC}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = GLMModel)
#'
GLMModel <- function(family = NULL, quasi = FALSE, ...) {

  MLModel(

    name = "GLMModel",
    label = "Generalized Linear Models",
    packages = c("MASS", "nnet", "stats"),
    response_types = c("BinomialVariate", "factor", "matrix",
                       "NegBinomialVariate", "numeric", "PoissonVariate"),
    weights = TRUE,
    predictor_encoding = "model.matrix",
    params = new_params(environment(), ...),

    fit = function(formula, data, weights, family = NULL, quasi, ...) {
      if (is.null(family)) {
        quasi_prefix <- function(x) if (quasi) paste0("quasi", x) else x
        y <- response(data)
        family <- switch_class(y,
          "BinomialVariate" = quasi_prefix("binomial"),
          "factor" = if (nlevels(y) <= 2) {
            quasi_prefix("binomial")
          } else {
            "multinom"
          },
          "matrix" = "mgaussian",
          "NegBinomialVariate" = "negbin",
          "numeric" = "gaussian",
          "PoissonVariate" = quasi_prefix("poisson")
        )
      }
      data <- as.data.frame(data)
      control <- stats::glm.control(...)
      if (identical(family, "mgaussian")) {
        stats::lm(formula, data = data, weights = weights)
      } else if (identical(family, "multinom")) {
        nnet::multinom(
          formula, data = data, weights = weights, maxit = 4 * control$maxit,
          trace = control$trace, reltol = control$epsilon
        )
      } else if (identical(family, "negbin")) {
        model_fit <- MASS::glm.nb(formula, data = data, weights = weights,
                                  control = control)
        MASS::glm.convert(model_fit)
      } else {
        stats::glm(formula, data = data, weights = weights, family = family,
                   control = control)
      }
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata,
              type = if (is(object, "multinom")) "probs" else "response")
    },

    varimp = function(object, base = exp(1), ...) {
      varimp_pval(object, base = base)
    }

  )

}

MLModelFunction(GLMModel) <- NULL


#' @rdname GLMModel
#'
#' @param direction mode of stepwise search, can be one of \code{"both"}
#'   (default), \code{"backward"}, or \code{"forward"}.
#' @param scope defines the range of models examined in the stepwise search.
#'   This should be a list containing components \code{upper} and \code{lower},
#'   both formulae.
#' @param k multiple of the number of degrees of freedom used for the penalty.
#'   Only \code{k = 2} gives the genuine AIC; \code{k = .(log(nobs))} is
#'   sometimes referred to as BIC or SBC.
#' @param trace if positive, information is printed during the running of
#'   \code{stepAIC}. Larger values may give more information on the fitting
#'   process.
#' @param steps maximum number of steps to be considered.
#'
GLMStepAICModel <- function(
  family = NULL, quasi = FALSE, ...,
  direction = c("both", "backward", "forward"), scope = list(), k = 2,
  trace = FALSE, steps = 1000
) {

  direction <- match.arg(direction)

  params <- new_params(environment())
  stepmodel <- GLMModel(family = family, quasi = quasi, ...)
  params <- params[setdiff(names(params), names(stepmodel@params))]

  MLModel(

    name = "GLMStepAICModel",
    label = "Generalized Linear Models (Stepwise)",
    packages = stepmodel@packages,
    response_types = c("binary", "BinomialVariate","NegBinomialVariate",
                       "numeric", "PoissonVariate"),
    weights = TRUE,
    predictor_encoding = stepmodel@predictor_encoding,
    params = c(stepmodel@params, params),

    fit = function(
      formula, data, weights, family = NULL, quasi, ...,
      direction, scope = list(), k, trace, steps
    ) {
      environment(formula) <- environment()
      if (is.null(family)) {
        quasi_prefix <- function(x) if (quasi) paste0("quasi", x) else x
        family <- switch_class(response(data),
          "BinomialVariate" = quasi_prefix("binomial"),
          "factor" = quasi_prefix("binomial"),
          "NegBinomialVariate" = "negbin",
          "numeric" = "gaussian",
          "PoissonVariate" = quasi_prefix("poisson")
        )
      }
      stepargs <- stepAIC_args(formula, direction, scope)
      data <- as.data.frame(data)
      control <- stats::glm.control(...)
      if (family == "negbin") {
        model_fit <- MASS::stepAIC(
          MASS::glm.nb(stepargs$formula, data = data, weights = weights,
                       control = control),
          direction = direction, scope = stepargs$scope, k = k, trace = trace,
          steps = steps
        )
        MASS::glm.convert(model_fit)
      } else {
        MASS::stepAIC(
          stats::glm(stepargs$formula, data = data, weights = weights,
                     family = family, control = control),
          direction = direction, scope = stepargs$scope, k = k, trace = trace,
          steps = steps
        )
      }
    },

    predict = stepmodel@predict,

    varimp = stepmodel@varimp

  )

}

MLModelFunction(GLMStepAICModel) <- NULL
