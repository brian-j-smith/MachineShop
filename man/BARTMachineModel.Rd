% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ML_BARTMachineModel.R
\name{BARTMachineModel}
\alias{BARTMachineModel}
\title{Bayesian Additive Regression Trees Model}
\usage{
BARTMachineModel(
  num_trees = 50,
  num_burn = 250,
  num_iter = 1000,
  alpha = 0.95,
  beta = 2,
  k = 2,
  q = 0.9,
  nu = 3,
  mh_prob_steps = c(2.5, 2.5, 4)/9,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{num_trees}{number of trees to be grown in the sum-of-trees model.}

\item{num_burn}{number of MCMC samples to be discarded as "burn-in".}

\item{num_iter}{number of MCMC samples to draw from the posterior
distribution.}

\item{alpha, beta}{base and power hyperparameters in tree prior for whether a
node is nonterminal or not.}

\item{k}{regression prior probability that \eqn{E(Y|X)} is
contained in the interval \eqn{(y_{min}, y_{max})}, based on a normal
distribution.}

\item{q}{quantile of the prior on the error variance at which the data-based
estimate is placed.}

\item{nu}{regression degrees of freedom for the inverse \eqn{sigma^2} prior.}

\item{mh_prob_steps}{vector of prior probabilities for proposing changes to
the tree structures: (GROW, PRUNE, CHANGE).}

\item{verbose}{logical indicating whether to print progress information about
the algorithm.}

\item{...}{additional arguments to \code{\link[bartMachine]{bartMachine}}.}
}
\value{
\code{MLModel} class object.
}
\description{
Builds a BART model for regression or classification.
}
\details{
\describe{
  \item{Response types:}{\code{binary factor}, \code{numeric}}
  \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
    \code{alpha}, \code{beta}, \code{k}, \code{nu}
  }
}

Further model details can be found in the source link below.

In calls to \code{\link{varimp}} for \code{BARTMachineModel}, argument
\code{type} may be specified as \code{"splits"} (default) for the
proportion of time each predictor is chosen for a splitting rule or as
\code{"trees"} for the proportion of times each predictor appears in a tree.
Argument \code{num_replicates} is also available to control the number of
BART replicates used in estimating the inclusion proportions [default: 5].
Variable importance is automatically scaled to range from 0 to 100.  To
obtain unscaled importance values, set \code{scale = FALSE}.  See example
below.
}
\examples{
\donttest{
## Requires prior installation of suggested package bartMachine to run

model_fit <- fit(sale_amount ~ ., data = ICHomes, model = BARTMachineModel)
varimp(model_fit, method = "model", type = "splits", num_replicates = 20,
       scale = FALSE)
}

}
\seealso{
\code{\link[bartMachine]{bartMachine}}, \code{\link{fit}},
\code{\link{resample}}
}
