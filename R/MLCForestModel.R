CForestModel <- function(control = NULL) {
  MLModel(
    name = "CForestModel",
    packages = "party",
    responses = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      party::cforest(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("CForestFit", CForestModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      object <- asParentFit(object)
      if(object@responses@is_censored) {
        if(length(times)) {
          predict(object, newdata = newdata, type = "prob") %>%
            lapply(function(fit) predict(fit, times)) %>%
            (function(args) do.call(rbind, args))
        } else {
          log(2) / predict(object, newdata = newdata, type = "response")
        }
      } else {
        predict(object, newdata = newdata, type = "prob") %>%
          unlist %>%
          matrix(nrow = nrow(newdata), byrow = TRUE) %>%
          drop
      }
    }
  )
}
