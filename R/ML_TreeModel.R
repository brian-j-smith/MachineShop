#' Classification and Regression Tree Models
#' 
#' A tree is grown by binary recursive partitioning using the response in the
#' specified formula and choosing splits from the terms of the right-hand-side.
#' 
#' @param mincut minimum number of observations to include in either child node.
#' @param minsize smallest allowed node size: a weighted quantity.
#' @param mindev within-node deviance must be at least this times that of the
#' root node for the node to be split.
#' @param split splitting criterion to use.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}}
#' }
#' 
#' Further model details can be found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[tree]{tree}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = TreeModel())
#'
TreeModel <- function(mincut = 5, minsize = 10, mindev = 0.01,
                      split = c("deviance", "gini")) {
  MLModel(
    name = "TreeModel",
    packages = "tree",
    types = c("factor", "numeric"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, split, ...) {
      environment(formula) <- environment()
      tree::tree(formula, data = data, weights = weights, split = split, ...)
    },
    predict = function(object, newdata, times, ...) {
      predict(object, newdata = newdata)
    }
  )
}
