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
    label = "Recursive Partitioning and Regression Trees",
    packages = c("rpart", "partykit"),
    types = c("factor", "numeric", "Surv"),
    params = list(control = as.call(c(.(list), params(environment())))),
    grid = function(x, length, ...) {
      cptable <- fit(x, model = RPartModel(cp = 0))$cptable[, "CP"]
      list(
        cp = seq(min(cptable), max(cptable), length = length)
      )
    },
    design = "terms",
    fit = function(formula, data, weights, ...) {
      method <- switch_class(response(formula, data),
                             "factor" = "class",
                             "numeric" = "anova",
                             "Surv" = "exp")
      rpart::rpart(formula, data = data, weights = weights, na.action = na.pass,
                   method = method, ...)
    },
    predict = function(object, newdata, fitbits, times, ...) {
      y <- response(fitbits)
      if (is.Surv(y)) {
        n <- length(times)
        if (n == 0) times <- surv_times(y)
        
        pred <- partykit::as.party(object) %>%
          predict(newdata = newdata, type = "prob") %>%
          sapply(function(fit) predict(fit, times)) %>% t
        if (n == 0) surv_mean(times, pred, surv_max(y)) else pred
      } else {
        predict(object, newdata = newdata)
      }
    },
    varimp = function(object, ...) {
      object$variable.importance
    }
  )
  
}
