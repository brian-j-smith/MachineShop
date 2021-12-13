setMethod("initialize", "MLInput",
  function(.Object, ..., id = make_id("input")) {
    callNextMethod(.Object, ..., id = id)
  }
)


NullInput <- function() {
  new("NullInput", id = "null")
}


update.MLInput <- function(object, ...) {
  object
}
