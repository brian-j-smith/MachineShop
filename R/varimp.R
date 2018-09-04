varimp <- function(object, scale = TRUE, ...) {
  new("VarImp", as(varimp_sub(object, ...), "VarImp"), scale = scale)
}


varimp_sub <- function(object, ...) {
  UseMethod("varimp_sub", object)
}


varimp_sub.MLModelFit <- function(object, ...) {
  requireModelNamespaces(field(object, ".packages"))
  varimp <- field(object, ".varimp")
  varimp(object, ...)
}
