#' Bayesian Additive Regression Trees Model
#'
#' Flexible nonparametric modeling of covariates for continuous, binary,
#' categorical and time-to-event outcomes.
#'
#' @param K if provided, then coarsen the times of survival responses per the
#'   quantiles \eqn{1/K, 2/K, ..., K/K} to reduce computational burdern.
#' @param sparse logical indicating whether to perform variable selection based
#'   on a sparse Dirichlet prior rather than simply uniform; see Linero 2016.
#' @param theta,omega \eqn{theta} and \eqn{omega} parameters; zero means
#'   random.
#' @param a,b sparse parameters for \eqn{Beta(a, b)} prior:
#'   \eqn{0.5 <= a <= 1} where lower values induce more sparsity and typically
#'   \eqn{b = 1}.
#' @param rho sparse parameter: typically \eqn{rho = p} where \eqn{p} is the
#'   number of covariates under consideration.
#' @param augment whether data augmentation is to be performed in sparse
#'   variable selection.
#' @param xinfo optional matrix whose rows are the covariates and columns their
#'   cutpoints.
#' @param usequants whether covariate cutpoints are defined by uniform quantiles
#'   or generated uniformly.
#' @param sigest normal error variance prior for numeric response variables.
#' @param sigdf degrees of freedom for error variance prior.
#' @param sigquant quantile at which a rough estimate of the error standard
#'   deviation is placed.
#' @param lambda scale of the prior error variance.
#' @param k number of standard deviations \eqn{f(x)}  is away from +/-3 for
#'   categorical response variables.
#' @param power,base power and base parameters for tree prior.
#' @param tau.num numerator in the \eqn{tau} definition, i.e.,
#'   \eqn{tau = tau.num / (k * sqrt(ntree))}.
#' @param offset override for the default \eqn{offset} of \eqn{F^-1(mean(y))}
#'   in the multivariate response probability
#'   \eqn{P(y[j] = 1 | x) = F(f(x)[j] + offset[j])}.
#' @param ntree number of trees in the sum.
#' @param numcut number of possible covariate cutoff values.
#' @param ndpost number of posterior draws returned.
#' @param nskip number of MCMC iterations to be treated as burn in.
#' @param keepevery interval at which to keep posterior draws.
#' @param printevery interval at which to print MCMC progress.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#' }
#'
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[BART]{gbart}}, \code{\link[BART]{mbart}},
#' \code{\link[BART]{surv.bart}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package BART to run
#'
#' fit(sale_amount ~ ., data = ICHomes, model = BARTModel)
#' }
#'
BARTModel <- function(
  K = NULL, sparse = FALSE, theta = 0, omega = 1, a = 0.5, b = 1, rho = NULL,
  augment = FALSE, xinfo = NULL, usequants = FALSE, sigest = NA, sigdf = 3,
  sigquant = 0.90, lambda = NA, k = 2, power = 2, base = 0.95, tau.num = NULL,
  offset = NULL, ntree = NULL, numcut = 100, ndpost = 1000, nskip = NULL,
  keepevery = NULL, printevery = 1000
) {

  MLModel(
    name = "BARTModel",
    label = "Bayesian Additive Regression Trees",
    packages = "BART",
    response_types = c("factor", "numeric", "Surv"),
    predictor_encoding = "model.matrix",
    params = new_params(environment()),
    fit = function(formula, data, weights, K = NULL, sigest = NA, sigdf = 3,
                   sigquant = 0.90, lambda = NA, ...) {
      x <- model.matrix(data, intercept = FALSE)
      y <- response(data)
      switch_class(y,
        "factor" = {
          throw(check_equal_weights(weights))
          if (nlevels(y) <= 2) {
            f <- BART::gbart
            y <- as.numeric(y) - 1
          } else {
            f <- BART::mbart
            y <- as.numeric(y)
          }
          f(x.train = x, y.train = y, type = "pbart", ...)
        },
        "numeric" = {
          BART::gbart(x.train = x, y.train = y, w = weights, sigest = sigest,
                      sigdf = sigdf, sigquant = sigquant, lambda = lambda, ...)
        },
        "Surv" = {
          throw(check_censoring(y, "right"))
          throw(check_equal_weights(weights))
          BART::surv.bart(x.train = x, times = y[, "time"],
                          delta = y[, "status"], K = K, ...)
        }
      )
    },
    predict = function(object, newdata, ...) {
      newx <- model.matrix(newdata, intercept = FALSE)
      if (is(object, "mbart")) {
        predict(object, newdata = newx)$prob.test.mean %>%
          matrix(nrow = nrow(newx), ncol = object$K, byrow = TRUE)
      } else if (is(object, "pbart")) {
        predict(object, newdata = newx)$prob.test.mean
      } else if (is(object, "survbart")) {
        N <- nrow(newx)
        K <- object$K
        newx <- cbind(t = object$times, newx[rep(seq_len(N), each = K), ])
        pred <- predict(object, newdata = newx)$surv.test.mean %>%
          matrix(nrow = N, ncol = K, byrow = TRUE)
        predict(Surv(object$times), pred, ...)
      } else if (is(object, "wbart")) {
        colMeans(predict(object, newdata = newx))
      }
    }
  )

}

MLModelFunction(BARTModel) <- NULL
