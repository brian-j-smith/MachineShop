#' Recursive Partitioning and Regression Tree Models
#' 
#' Fit an \code{rpart} model.
#' 
#' @param minsplit minimum number of observations that must exist in a node in
#'   order for a split to be attempted.
#' @param minbucket minimum number of observations in any terminal node.
#' @param cp complexity parameter.
#' @param maxcompete number of competitor splits retained in the output.
#' @param maxsurrogate number of surrogate splits retained in the output.
#' @param usesurrogate how to use surrogates in the splitting process.
#' @param xval number of cross-validations.
#' @param surrogatestyle controls the selection of a best surrogate.
#' @param maxdepth maximum depth of any node of the final tree, with the root
#'   node counted as depth 0.
#' 
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{cp}
#'   }
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
#' fit(Species ~ ., data = iris, model = RPartModel)
#'
RPartModel <- function(minsplit = 20, minbucket = round(minsplit / 3),
                       cp = 0.01, maxcompete = 4, maxsurrogate = 5,
                       usesurrogate = 2, xval = 10, surrogatestyle = 0,
                       maxdepth = 30) {
  
  MLModel(
    name = "RPartModel",
    label = "Recursive Partitioning and Regression Trees",
    packages = c("rpart", "partykit"),
    response_types = c("factor", "numeric", "Surv"),
    predictor_encoding = "terms",
    params = list(control = as.call(c(.(list), params(environment())))),
    grid = function(x, length, ...) {
      cptable <- fit(x, model = RPartModel(cp = 0))$cptable[, "CP"]
      list(
        cp = seq(min(cptable), max(cptable), length = length)
      )
    },
    fit = function(formula, data, weights, ...) {
      method <- switch_class(response(data),
                             "factor" = "class",
                             "numeric" = "anova",
                             "Surv" = "exp")
      rpart::rpart(formula, data = as.data.frame(data), weights = weights,
                   na.action = na.pass, method = method, ...)
    },
    predict = function(object, newdata, model, times, ...) {
      y <- response(model)
      newdata <- as.data.frame(newdata)
      if (is.Surv(y)) {
        object <- partykit::as.party(object)
        fits <- predict(object, newdata = newdata, type = "prob")
        predict(y, fits, times, ...)
      } else {
        predict(object, newdata = newdata)
      }
    },
    varimp = function(object, ...) {
      object$variable.importance
    }
  )
  
}

MLModelFunction(RPartModel) <- NULL
