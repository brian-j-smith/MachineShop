GBMModel <- function(distribution = NULL, n.trees = NULL,
                     interaction.depth = NULL, n.minobsinnode = NULL,
                     shrinkage = NULL, bag.fraction = NULL) {
  MLModel(
    name = "GBMModel",
    packages = "gbm",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      gbm::gbm(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("GBMFit", GBMModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      obs <- response(object)
      object <- asParentFit(object)
      if(object$distribution$name == "coxph") {
        if(length(times)) {
          lp <- predict(object, n.trees = object$n.trees, type = "link")
          newlp <- predict(object, newdata = newdata, n.trees = object$n.trees,
                           type = "link")
          cumhaz <- basehaz(obs, exp(lp), times)
          exp(exp(newlp - mean(lp)) %o% -cumhaz)
        } else {
          exp(predict(object, newdata = newdata, n.trees = object$n.trees,
                      type = "link"))
        }
      } else {
        predict(object, newdata = newdata, n.trees = object$n.trees,
                type = "response") %>% drop
      }
    }
  )
}
