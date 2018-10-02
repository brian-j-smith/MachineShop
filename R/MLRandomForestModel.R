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
#' @examples
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = RandomForestModel())
#' 
RandomForestModel <- function(ntree = 500,
                              mtry = .(if (is.factor(y)) floor(sqrt(nvars))
                                       else max(floor(nvars / 3), 1)),
                              replace = TRUE,
                              nodesize = .(if (is.factor(y)) 1 else 5),
                              maxnodes = NULL) {
  MLModel(
    name = "RandomForestModel",
    packages = "randomForest",
    types = c("factor", "numeric"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, ...) {
      if (!all(weights == 1)) warning("weights are unsupported and will be ignored")
      environment(formula) <- environment()
      randomForest::randomForest(formula, data = data, ...)
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
