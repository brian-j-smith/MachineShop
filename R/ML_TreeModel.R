#' Classification and Regression Tree Models
#'
#' A tree is grown by binary recursive partitioning using the response in the
#' specified formula and choosing splits from the terms of the right-hand-side.
#'
#' @param mincut minimum number of observations to include in either child node.
#' @param minsize smallest allowed node size: a weighted quantity.
#' @param mindev within-node deviance must be at least this times that of the
#'   root node for the node to be split.
#' @param split splitting criterion to use.
#' @param k scalar cost-complexity parameter defining a subtree to return.
#' @param best integer alternative to \code{k} requesting the number of terminal
#'   nodes of a subtree in the cost-complexity sequence to return.
#' @param method character string denoting the measure of node heterogeneity
#'   used to guide cost-complexity pruning.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}}
#' }
#'
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[tree]{tree}}, \code{\link[tree]{prune.tree}},
#' \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(Species ~ ., data = iris, model = TreeModel)
#'
TreeModel <- function(mincut = 5, minsize = 10, mindev = 0.01,
                      split = c("deviance", "gini"), k = NULL, best = NULL,
                      method = c("deviance", "misclass")) {

  split <- match.arg(split)
  method <- if (is.null(k) && is.null(best)) NULL else match.arg(method)

  MLModel(
    name = "TreeModel",
    label = "Regression and Classification Trees",
    packages = "tree",
    response_types = c("factor", "numeric"),
    predictor_encoding = "terms",
    params = params(environment()),
    fit = function(formula, data, weights, split, k = NULL, best = NULL,
                   method = NULL, ...) {
      modelfit <- tree::tree(formula, data = as.data.frame(data),
                             weights = weights, split = split, ...)
      if (!is.null(method)) {
        tree::prune.tree(modelfit, k = k, best = best, method = method)
      } else modelfit
    },
    predict = function(object, newdata, times, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata)
    }
  )

}

MLModelFunction(TreeModel) <- NULL
