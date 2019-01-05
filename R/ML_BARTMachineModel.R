#' Bayesian Additive Regression Trees Model
#'
#' Builds a BART model for regression or classification.
#'
#' @param num_trees number of trees to be grown in the sum-of-trees model.
#' @param num_burn number of MCMC samples to be discarded as "burn-in".
#' @param num_iter number of MCMC samples to draw from the posterior
#' distribution.
#' @param alpha,beta base and power hyperparameters in tree prior for whether a
#' node is nonterminal or not.
#' @param k regression prior probability that \eqn{E(Y|X)} is
#' contained in the interval \eqn{(y_{min}, y_{max})}, based on a normal
#' distribution.
#' @param q quantile of the prior on the error variance at which the data-based
#' estimate is placed. 
#' @param nu regression degrees of freedom for the inverse \eqn{X^2} prior.
#' @param mh_prob_steps vector of prior probabilities for proposing changes to
#' the tree structures: (GROW, PRUNE, CHANGE).
#' @param verbose logical indicating whether to print progress information about
#' the algorithm.
#' @param ... additional arguments to \code{\link[bartMachine]{bartMachine}}.
#' 
#' @details 
#' \describe{
#' \item{Response Types:}{\code{binary}, \code{numeric}}
#' }
#' 
#' Further model details can be found in the source link below.
#' 
#' In calls to \code{\link{varimp}} for \code{BARTMachineModel}, argument
#' \code{metric} may be spedified as \code{"splits"} (default) for the
#' proportion of time each predictor is chosen for a splitting rule or as
#' \code{"trees"} for the proportion of times each predictor appears in a tree.
#' Argument \code{num_replicates} is also available to control the number of
#' BART replicates used in estimating the inclusion proportions [default: 5].
#' Variable importance is automatically scaled to range from 0 to 100.  To
#' obtain unscaled importance values, set \code{scale = FALSE}.  See example
#' below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[bartMachine]{bartMachine}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' \donttest{
#' library(MASS)
#' 
#' modelfit <- fit(medv ~ ., data = Boston, model = BARTMachineModel())
#' varimp(modelfit, metric = "splits", num_replicates = 20, scale = FALSE)
#' }
#'
BARTMachineModel <- function(num_trees = 50, num_burn = 250, num_iter = 1000,
                             alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3,
                             mh_prob_steps = c(2.5, 2.5, 4) / 9,
                             verbose = FALSE, ...) {

  MLModel(
    name = "BARTMachineModel",
    label = "Bayesian Additive Regression Trees",
    packages = "bartMachine",
    types = c("binary", "numeric"),
    params = params(environment()),
    design = "model.matrix",
    fit = function(formula, data, weights, ...) {
      assert_equal_weights(weights)
      terms <- extract(formula, data)
      bartMachine::bartMachine(as.data.frame(terms$x), terms$y, ...)
    },
    predict = function(object, newdata, fitbits, ...) {
      newx <- extract(formula(fitbits)[-2], newdata)$x
      predict(object, new_data = as.data.frame(newx))
    },
    varimp = function(object, metric = c("splits", "trees"), num_replicates = 5,
                      ...) {
      bartMachine::investigate_var_importance(object,
                                              type = match.arg(metric),
                                              num_replicates = num_replicates,
                                              plot = FALSE)$avg_var_props
    }
  )
  
}
