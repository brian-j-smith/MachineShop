print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x))
}


print.Resamples <- function(x, ...) {
  show(x)
}


setMethod("show", "MLControl",
  function(object) {
    if (length(object@surv_times)) {
      cat("Survival times:", toString(object@surv_times), "\n")
    }
    cat("Seed:", object@seed, "\n\n")
    invisible()
  }
)


setMethod("show", "BootMLControl",
  function(object) {
    cat("An object from class \"MLControl\"\n\n",
        "Name: BootControl\n",
        "Label: Bootstrap Resampling\n",
        "Samples: ", object@samples, "\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "CVMLControl",
  function(object) {
    cat("An object from class \"MLControl\"\n\n",
        "Name: CVControl\n",
        "Label: K-Fold Cross-Validation\n",
        "Folds: ", object@folds, "\n",
        "Repeats: ", object@repeats, "\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "OOBMLControl",
  function(object) {
    cat("An object from class \"MLControl\"\n\n",
        "Name: OOBControl\n",
        "Label: Out-Of-Bootstrap Resampling\n",
        "Samples: ", object@samples, "\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "SplitMLControl",
  function(object) {
    cat("An object from class \"MLControl\"\n\n",
        "Name: SplitControl\n",
        "Label: Split Training and Test Samples\n",
        "Training proportion: ", object@prop, "\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "TrainMLControl",
  function(object) {
    cat("An object from class \"MLControl\"\n\n",
        "Name: TrainControl\n",
        "Label: Training Resubstitution\n",
        sep = "")
    callNextMethod(object)
    invisible()
  }
)


setMethod("show", "MLMetric",
  function(object) {
    info <- metricinfo(object)[[1]]
    cat("An object of class \"", class(object), "\"\n\n",
        "Metric name: ", info$name, "\n",
        "Label: ", info$label, "\n",
        "Maximize: ", info$maximize, "\n\n",
        sep = "")
    cat("Arguments:\n")
    print(info$arguments)
    cat("\nTypes:\n")
    print(info$types)
  }
)


setMethod("show", "MLModel",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "Model name: ", object@name, "\n",
        "Label: ", object@label, "\n",
        "Packages: ", toString(object@packages), "\n",
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
    cat("Grid:\n")
    print(object@grid)
    cat("\n")
    print(object@performance)
    model_names <- dimnames(object@performance)[[3]]
    if (length(model_names) > 1) {
      cat("Selected (", names(object@selected), "): ",
          model_names[object@selected], "\n\n",
          sep = "")
    }
  }
)


setMethod("show", "HTestPerformanceDiff",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "Upper diagonal: mean differences (row - column)\n",
        "Lower diagonal: p-values\n",
        "P-value adjustment method: ", object@adjust, "\n\n",
        sep = "")
    print(object@.Data)
  }
)


setMethod("show", "Performance",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n", sep = "")
    cat("Metrics:", toString(dimnames(object)[[2]]), "\n")
    if (length(dim(object)) > 2) {
      cat("Models:", toString(dimnames(object)[[3]]), "\n")
    }
    cat("\n")
  }
)


setMethod("show", "Resamples",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "Models: ", toString(levels(object$Model)), "\n", sep = "")
    if (length(object@strata)) {
      cat("Stratification variable:", object@strata, "\n")
    }
    cat("\n")
    show(object@control)
    invisible()
  }
)


setMethod("show", "SummaryConfusion",
  function(object) {
    n <- object@N
    acc <- object@Accuracy
    cat("Number of responses: ", n, "\n",
        "Accuracy (SE): ", acc, " (", sqrt(acc * (1 - acc) / n), ")\n",
        "Majority class: ", object@Majority, "\n",
        "Kappa: ", object@Kappa, "\n\n",
        sep = "")
    print(object@.Data)
  }
)
