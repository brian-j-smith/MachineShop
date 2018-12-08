#' Gradient Boosting with Regression Trees
#' 
#' Gradient boosting for optimizing arbitrary loss functions where regression
#' trees are utilized as base-learners.
#' 
#' @param family \code{\link[mboost]{Family}} object.  Set automatically
#' according to the class type of the response variable.
#' @param mstop number of initial boosting iterations.
#' @param nu step size or shrinkage parameter between 0 and 1.
#' @param risk method to use in computing the empirical risk for each boosting
#' iteration.
#' @param stopintern logical inidicating whether the boosting algorithm stops
#' internally when the out-of-bag risk increases at a subsequent iteration.
#' @param trace logical indicating whether status information is printed during
#' the fitting process.
#' @param teststat type of the test statistic to be applied for variable
#' selection.
#' @param testtype how to compute the distribution of the test statistic.
#' @param mincriterion value of the test statistic or 1 - p-value that must be
#' exceeded in order to implement a split.
#' @param minsplit minimum sum of weights in a node in order to be considered
#' for splitting.
#' @param minbucket minimum sum of weights in a terminal node.
#' @param maxdepth maximum depth of the tree.
#' @param saveinfo logical indicating whether to store information about
#' variable selection in \code{info} slot of each \code{partynode}.
#' @param ... additional arguments to \code{\link[partykit]{ctree_control}}.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{binary}, \code{numeric}, \code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[mboost]{blackboost}}, \code{\link[mboost]{Family}},
#' \code{\link[partykit]{ctree_control}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(type ~ ., data = Pima.tr, model = BlackBoostModel())
#'
BlackBoostModel <- function(family = NULL, mstop = 100, nu = 0.1,
                            risk = c("inbag", "oobag", "none"),
                            stopintern = FALSE, trace = FALSE,
                            teststat = c("quadratic", "maximum"),
                            testtype = c("Teststatistic", "Univariate",
                                         "Bonferroni", "MonteCarlo"),
                            mincriterion = 0, minsplit = 10, minbucket = 4,
                            maxdepth = 2, saveinfo = FALSE, ...) {
  
  teststat <- match.arg(teststat)
  testtype <- match.arg(testtype)
  
  args <- params(environment())
  mainargs <- names(args) %in% "family"
  controlargs <- names(args) %in% c("mstop", "nu", "risk", "stopintern",
                                    "trace")

  params <- args[mainargs]
  params$control <- as.call(c(.(mboost::boost_control), args[controlargs]))
  params$tree_controls <- as.call(c(.(partykit::ctree_control),
                                    args[!(mainargs | controlargs)]))

  MLModel(
    name = "BlackBoostModel",
    packages = c("mboost", "partykit"),
    types = c("binary", "numeric", "Surv"),
    params = params,
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, family = NULL, ...) {
      environment(formula) <- environment()
      if (is.null(family)) {
        family <- switch_class(response(formula, data),
                               "factor" = mboost::Binomial(),
                               "numeric" = mboost::Gaussian(),
                               "Surv" = mboost::CoxPH())
      }
      mboost::blackboost(formula, data = data, na.action = na.pass,
                         weights = weights, family = family, ...)
    },
    predict = function(object, newdata, times, ...) {
      if (object$family@name == "Cox Partial Likelihood") {
        newlp <- predict(object, newdata = newdata, type = "link") %>% drop
        if (length(times)) {
          y <- object$response
          lp <- predict(object, type = "link") %>% drop
          cumhaz <- basehaz(y, exp(lp), times)
          exp(exp(newlp) %o% -cumhaz)
        } else {
          exp(newlp)
        }
      } else {
        predict(object, newdata = newdata, type = "response")
      }
    },
    varimp = function(object, ...) {
      structure(mboost::varimp(object), class = "numeric")
    }
  )
  
}
