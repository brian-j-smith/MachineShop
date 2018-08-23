resample <- function(formula, data, model, control) {
  .resample(control, formula, data, model)
}


setGeneric(".resample", function(object, ...) standardGeneric(".resample"))


setMethod(".resample", "CVControl",
  function(object, formula, data, model) {
    foldids <- createMultiFolds(eval(formula[[2]], data),
                                k = object@folds,
                                times = object@repeats)
    foreach(foldid = foldids,
            .packages = c("survival", "MLModels"),
            .combine = "rbind") %dopar% {
              validate(formula, data[foldid,], data[-foldid,], model, object)
            } %>%
      as.data.frame %>%
      structure(class = c("Resamples", "data.frame"))
  }
)


validate <- function(formula, train, test, model, control) {
  mfit <- fit(formula, train, model)
  pred <- model@predict(object = mfit, data = test, type = "prob",
                        times = control@survtimes)
  obs <- eval(formula[[2]], test)
  do.call(control@summary,
          c(list(observed = obs, predicted = pred), as(control, "list")))
}
