setGeneric("resample", function(object, x, ...) standardGeneric("resample"))


setMethod("resample", c("formula", "data.frame"),
  function(object, x, model, control) {
    resample(control, object, x, model)
  }
)


setMethod("resample", c("CVControl", "formula"),
  function(object, x, data, model) {
    foldids <- createMultiFolds(eval(x[[2]], data),
                                k = object@folds,
                                times = object@repeats)
    foreach(foldid = foldids,
            .packages = c("survival", "MLModels"),
            .combine = "rbind") %dopar% {
              validate(x, data[foldid,], data[-foldid,], model, object)
            } %>%
      as.data.frame %>%
      structure(class = c("Resamples", "data.frame"))
  }
)


validate <- function(formula, train, test, model, control) {
  mfit <- fit(formula, train, model)
  pred <- predict(object = mfit, data = test, type = "prob",
                  times = control@survtimes)
  obs <- eval(formula[[2]], test)
  do.call(control@summary,
          c(list(observed = obs, predicted = pred), as(control, "list")))
}
