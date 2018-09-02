RandomForestModel <- function(ntree = NULL, mtry = NULL, replace = NULL,
                              nodesize = NULL, maxnodes = NULL) {
  MLModel(
    name = "RandomForestModel",
    packages = "randomForest",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, ...) {
      environment(formula) <- environment()
      randomForest::randomForest(formula, data = data, ...) %>%
        asMLModelFit("RandomForestFit", RandomForestModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(asParentFit(object), newdata = newdata,
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
