#' Gradient Boosting with Regression Trees
#'
#' Gradient boosting for optimizing arbitrary loss functions where regression
#' trees are utilized as base-learners.
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
#' @param teststat type of the test statistic to be applied for variable
#'   selection.
#' @param testtype how to compute the distribution of the test statistic.
#' @param mincriterion value of the test statistic or 1 - p-value that must be
#'   exceeded in order to implement a split.
#' @param minsplit minimum sum of weights in a node in order to be considered
#'   for splitting.
#' @param minbucket minimum sum of weights in a terminal node.
#' @param maxdepth maximum depth of the tree.
#' @param saveinfo logical indicating whether to store information about
#'   variable selection in \code{info} slot of each \code{partynode}.
#' @param ... additional arguments to \code{\link[partykit]{ctree_control}}.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{binary factor}, \code{BinomialVariate},
#'     \code{NegBinomialVariate}, \code{numeric}, \code{PoissonVariate},
#'     \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{mstop}, \code{maxdepth}
#'   }
#' }
#'
#' Default values and further model details can be found in the source links
#' below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[mboost]{blackboost}}, \code{\link[mboost]{Family}},
#' \code{\link[partykit]{ctree_control}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested packages mboost and partykit to run
#'
#' data(Pima.tr, package = "MASS")
#'
#' fit(type ~ ., data = Pima.tr, model = BlackBoostModel)
#' }
#'
BlackBoostModel <- function(
  family = NULL, mstop = 100, nu = 0.1, risk = c("inbag", "oobag", "none"),
  stopintern = FALSE, trace = FALSE, teststat = c("quadratic", "maximum"),
  testtype = c("Teststatistic", "Univariate", "Bonferroni", "MonteCarlo"),
  mincriterion = 0, minsplit = 10, minbucket = 4, maxdepth = 2,
  saveinfo = FALSE, ...
) {

  risk <- match.arg(risk)
  teststat <- match.arg(teststat)
  testtype <- match.arg(testtype)

  MLModel(

    name = "BlackBoostModel",
    label = "Gradient Boosting with Regression Trees",
    packages = c("mboost", "partykit"),
    response_types = c("binary", "BinomialVariate", "NegBinomialVariate",
                       "numeric", "PoissonVariate", "Surv"),
    weights = TRUE,
    predictor_encoding = "model.frame",
    params = new_params(environment(), ...),

    gridinfo = new_gridinfo(
      param = c("mstop", "maxdepth"),
      get_values = c(
        function(n, ...) round(seq_range(0, 50, c(1, 1000), n + 1)),
        function(n, ...) seq_len(min(n, 10))
      )
    ),

    fit = function(
      formula, data, weights, family = NULL, mstop, nu, risk, stopintern,
      trace, ...
    ) {
      if (is.null(family)) {
        family <- switch_class(response(data),
          "BinomialVariate" = mboost::Binomial(type = "glm"),
          "factor" = mboost::Binomial(),
          "NegBinomialVariate" = mboost::NBinomial(),
          "numeric" = mboost::Gaussian(),
          "PoissonVariate" = mboost::Poisson(),
          "Surv" = mboost::CoxPH()
        )
      }
      mboost::blackboost(
        formula, data = as.data.frame(formula, data), na.action = na.pass,
        weights = weights, family = family,
        control = mboost::boost_control(
          mstop = mstop, nu = nu, risk = risk, stopintern = stopintern,
          trace = trace
        ),
        tree_controls = partykit::ctree_control(...)
      )
    },

    predict = function(object, newdata, model, ...) {
      newdata <- as.data.frame(newdata)
      if (object$family@name == "Cox Partial Likelihood") {
        lp <- drop(predict(object, type = "link"))
        new_lp <- drop(predict(object, newdata = newdata, type = "link"))
        predict(object$response, lp, new_lp, weights = case_weights(model), ...)
      } else {
        predict(object, newdata = newdata, type = "response")
      }
    },

    varimp = function(object, ...) {
      structure(mboost::varimp(object), class = "numeric")
    }

  )

}

MLModelFunction(BlackBoostModel) <- NULL
