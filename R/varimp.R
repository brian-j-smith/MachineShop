varimp <- function(object, scale = TRUE, ...) {
  varimp <- if(isS4(object)) object@.varimp else object$.varimp
  new("VarImp", as(varimp(object, ...), "VarImp"), scale = scale)
}
