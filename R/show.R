print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x))
}


setMethod("show", "MLControl",
  function(object) {
    cat("Class cutoff probability:", object@cutoff, "\n\n")
    if (length(object@surv_times)) {
      cat("Survival times:", toString(object@surv_times), "\n\n")
    }
    cat("Omit missing responses: ", object@na.rm, "\n\n",
        "Seed: ", object@seed, "\n\n", sep = "")
    invisible()
  }
)


setMethod("show", "BootMLControl",
  function(object) {
    cat("Resamples control object of class \"", class(object), "\"\n\n",
        "Method: Bootstrap Resampling\n\n",
        "Samples: ", object@samples, "\n\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "CVMLControl",
  function(object) {
    cat("Resamples control object of class \"", class(object), "\"\n\n",
        "Method: K-Fold Cross-Validation\n\n",
        "Folds: ", object@folds, "\n\n",
        "Repeats: ", object@repeats, "\n\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "OOBMLControl",
  function(object) {
    cat("Resamples control object of class \"", class(object), "\"\n\n",
        "Method: Out-Of-Bootstrap Resampling\n\n",
        "Samples: ", object@samples, "\n\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "SplitMLControl",
  function(object) {
    cat("Resamples control object of class \"", class(object), "\"\n\n",
        "Method: Split Training and Test Samples\n\n",
        "Training proportion: ", object@prop, "\n\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "TrainMLControl",
  function(object) {
    cat("Resamples control object of class \"", class(object), "\"\n\n",
        "Method: Training Resubstitution\n\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "MLModel",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "Name: ", object@name, "\n\n",
        "Required packages: ", toString(object@packages), "\n\n",
        "Response types: ", toString(object@types), "\n\n",
        "Parameters:\n",
        sep = "")
    print(object@params)
    if (length(object@params) == 0) cat("\n")
    invisible()
  }
)


setMethod("show", "MLModelFit",
  function(object) {
    show(unMLModelFit(object))
  }
)


setMethod("show", "MLModelTune",
  function(object) {
    callNextMethod(object)
    cat("grid:\n")
    print(object@grid)
    cat("\nresamples:\n")
    print(object@resamples)
    model_names <- levels(object@resamples@response$Model)
    if (length(model_names) > 1) {
      cat("Selected: ", model_names[object@selected],
          " (", names(object@selected), ")\n\n", sep = "")
    }
  }
)


setMethod("show", "Resamples",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "Models: ", toString(levels(object@response$Model)), "\n\n",
        "Metrics: ", toString(dimnames(object)[[2]]), "\n\n", sep = "")
    if (length(object@strata)) {
      cat("Stratification variable:", object@strata, "\n\n")
    }
    show(object@control)
    invisible()
  }
)


setMethod("show", "ResamplesHTest",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "Upper diagonal: mean differences (row - column)\n",
        "Lower diagonal: p-values\n",
        "P-value adjustment method: ", object@adjust, "\n\n",
        sep = "")
    print(object@.Data)
  }
)
