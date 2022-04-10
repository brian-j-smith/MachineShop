setMethod("initialize", "MLInput",
  function(.Object, ..., id = make_id("input")) {
    callNextMethod(.Object, ..., id = id)
  }
)


update.MLInput <- function(object, ...) {
  object
}
