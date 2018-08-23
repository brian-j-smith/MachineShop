resample <- function(formula, data, model, control) {
  .resample(control, formula, data, model)
}


setGeneric(".resample", function(object, ...) standardGeneric(".resample"))


setMethod(".resample", "CVControl",
  function(object, formula, data, model) {
    foldids <- model.frame(formula, data, na.action = NULL) %>%
      model.response() %>%
      createMultiFolds(k = object$folds, times = object$repeats)
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
                        times = control$survtimes)
  obs <- model.response(model.frame(formula, test, na.action = NULL))
  do.call(control$summary,
          c(list(observed = obs, predicted = pred), as.list(control)))
}
