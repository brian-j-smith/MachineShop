setMethod("show", "MLModel",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "name: ", object@name, "\n\n",
        "packages: ", toString(object@packages), "\n\n",
        "types: ", toString(object@types), "\n\n",
        "params:\n",
        sep = "")
    print(object@params)
    if (length(object@params) == 0) cat("\n")
    invisible()
  }
)


setMethod("show", "MLModelTune",
  function(object) {
    callNextMethod(object)
    cat("grid:\n")
    print(object@grid)
    cat("\nresamples:\n")
    print(object@resamples)
    if (length(dim(object@resamples)) > 2) {
      cat("selected: Model", object@selected, " (", names(object@selected),
          ")\n\n", sep = "")
    }
  }
)


setMethod("show", "Resamples",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n", sep = "")
    dns <- dimnames(object)
    if (length(dns) > 2) cat("models: ", toString(dns[[3]]), "\n\n", sep = "")
    cat("metrics: ", toString(dns[[2]]), "\n\n",
        "method: ", object@method, "\n\n",
        "resamples: ", dim(object)[1], "\n\n", sep = "")
    invisible()
  }
)


setMethod("show", "ResamplesHTest",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "upper diagonal: mean differences (row - column)\n",
        "lower diagonal: p-values\n",
        "p-value adjustment: ", object@adjust, "\n\n",
        sep = "")
    print(object@.Data)
  }
)
