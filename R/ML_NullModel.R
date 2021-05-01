NullModel <- function() {
  MLModel(
    name = "NullModel",
    label = "Null Model",
    response_types = settings("response_types"),
    fit = function(...) throw(LocalError("no specified model to fit"))
  )
}
