NullInput <- function() {
  new("NullInput", id = character())
}


NullModel <- function() {
  MLModel(
    id = character(),
    name = "NullModel",
    label = "Null Model",
    response_types = settings("response_types"),
    fit = function(...) throw(LocalError("no specified model to fit"))
  )
}
