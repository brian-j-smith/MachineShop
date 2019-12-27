NullModel <- MLModel(
  name = "NullModel",
  label = "Null Model",
  response_types = .response_types,
  fit = function(...) stop("no specified model to fit", call. = FALSE)
)
