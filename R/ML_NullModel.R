NullModel <- MLModel(
  name = "NullModel",
  label = "Null Model",
  response_types = settings("response_types"),
  fit = function(...) stop("no specified model to fit", call. = FALSE)
)
