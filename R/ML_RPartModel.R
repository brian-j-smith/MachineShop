#' Recursive Partitioning and Regression Tree Models
#' 
#' Fit an \code{rpart} model.
#' 
#' @param minsplit minimum number of observations that must exist in a node in
#' order for a split to be attempted.
#' @param minbucket minimum number of observations in any terminal node.
#' @param cp complexity parameter.
#' @param maxcompete number of competitor splits retained in the output.
#' @param maxsurrogate number of surrogate splits retained in the output.
#' @param usesurrogate how to use surrogates in the splitting process.
#' @param xval number of cross-validations.
#' @param surrogatestyle controls the selection of a best surrogate.
#' @param maxdepth maximum depth of any node of the final tree, with the root
#' node counted as depth 0.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#' }
#' 
#' Further model details can be found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[rpart]{rpart}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = RPartModel())
#'
RPartModel <- function(minsplit = 20, minbucket = round(minsplit / 3),
                       cp = 0.01, maxcompete = 4, maxsurrogate = 5,
                       usesurrogate = 2, xval = 10, surrogatestyle = 0,
                       maxdepth = 30) {
  
  MLModel(
    name = "RPartModel",
    packages = c("rpart", "partykit"),
    types = c("factor", "numeric", "Surv"),
    params = list(control = as.call(c(.(list), params(environment())))),
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      method <- switch_class(response(formula, data),
                             "factor" = "class",
                             "numeric" = "anova",
                             "Surv" = "exp")
      rpart::rpart(formula, data = data, weights = weights, na.action = na.pass,
                   method = method, ...)
    },
    predict = function(object, newdata, fitbits, times, ...) {
      if (is.Surv(response(fitbits)) && length(times)) {
        predict(partykit::as.party(object), newdata = newdata,
                type = "prob") %>%
          lapply(function(fit) predict(fit, times)) %>%
          (function(args) do.call(rbind, args))
      } else {
        predict(object, newdata = newdata)
      }
    },
    varimp = function(object, ...) {
      object$variable.importance
    }
  )
  
}
