varimp <- function(object, scale = TRUE, ...) {
  varimp <- field(object, ".varimp")
  new("VarImp", as(varimp(object, ...), "VarImp"), scale = scale)
}
