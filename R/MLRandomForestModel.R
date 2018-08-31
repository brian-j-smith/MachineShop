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
    predict = function(object, newdata, type = "response", cutoff = 0.5, ...) {
      object <- asParentFit(object)
      obs <- response(object)
      pred <- predict(object, newdata = newdata,
                      type = ifelse(is.factor(obs), "prob", "response"))
      if(type == "response") {
        pred <- convert(obs, pred, cutoff = cutoff)
      }
      pred
    }
  )
}
