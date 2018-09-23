#' Random Forest Model
#' 
#' Implementation of Breiman's random forest algorithm (based on Breiman and
#' Cutler's original Fortran code) for classification and regression.
#' 
#' @param ntree number of trees to grow.
#' @param mtry number of variables randomly sampled as candidates at each split.
#' @param replace should sampling of cases be done with or without replacement?
#' @param nodesize minimum size of terminal nodes.
#' @param maxnodes maximum number of terminal nodes trees in the forest can
#' have.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[randomForest]{randomForest}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
RandomForestModel <- function(ntree = NULL, mtry = NULL, replace = NULL,
                              nodesize = NULL, maxnodes = NULL) {
  MLModel(
    name = "RandomForestModel",
    packages = "randomForest",
    types = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights, ...) {
      if(!all(weights == 1)) warning("weights are unsupported and will be ignored")
      environment(formula) <- environment()
      randomForest::randomForest(formula, data = data, ...) %>%
        asMLModelFit("RandomForestFit", RandomForestModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(unMLModelFit(object), newdata = newdata,
              type = ifelse(is.factor(response(object)), "prob", "response"))
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      drop(randomForest::importance(object, ...))
    }
  )
}
