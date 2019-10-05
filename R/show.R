print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x))
}


print.Resamples <- function(x, ...) {
  show(x)
}


print.SurvMatrix <- function(x, ...) {
  show_title(x)
  print(as(x, "matrix"))
  cat("Attribute \"times\":\n")
  print(time(x))
}


str.SurvMatrix <- function(object, ...) {
  cat("'", class(object)[1], "'", sep = "")
  str(unclass(object), ...)
}


setMethod("show", "MLControl",
  function(object) {
    labels <- c("times" = "Survival times", "method" = "Method",
                "dist" = "Distribution")
    for (name in names(labels)) {
      x <- slot(object, name)
      if (length(x)) cat(labels[name], ": ", toString(x), "\n", sep = "")
    }
    cat("Seed:", object@seed, "\n\n")
    invisible()
  }
)


setMethod("show", "MLBootControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: BootControl\n",
        "Label: Bootstrap Resampling\n",
        "Samples: ", object@samples, "\n",
        sep = "")
    getMethod(show, "MLControl")(object)
    invisible()
  }
)


setMethod("show", "MLBootOptimismControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: BootOptimismControl\n",
        "Label: Optimism-Corrected Bootstrap Resampling\n",
        "Samples: ", object@samples, "\n",
        sep = "")
    getMethod(show, "MLControl")(object)
    invisible()
  }
)


setMethod("show", "MLCVControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: CVControl\n",
        "Label: K-Fold Cross-Validation\n",
        "Folds: ", object@folds, "\n",
        "Repeats: ", object@repeats, "\n",
        sep = "")
    getMethod(show, "MLControl")(object)
    invisible()
  }
)


setMethod("show", "MLOOBControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: OOBControl\n",
        "Label: Out-Of-Bootstrap Resampling\n",
        "Samples: ", object@samples, "\n",
        sep = "")
    getMethod(show, "MLControl")(object)
    invisible()
  }
)


setMethod("show", "MLSplitControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: SplitControl\n",
        "Label: Split Training and Test Samples\n",
        "Training proportion: ", object@prop, "\n",
        sep = "")
    getMethod(show, "MLControl")(object)
    invisible()
  }
)


setMethod("show", "MLTrainControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: TrainControl\n",
        "Label: Training Resubstitution\n",
        sep = "")
    getMethod(show, "MLControl")(object)
    invisible()
  }
)


setMethod("show", "MLMetric",
  function(object) {
    show_title(object)
    cat("\n",
        "Metric name: ", object@name, "\n",
        "Label: ", object@label, "\n",
        "Maximize: ", object@maximize, "\n\n",
        sep = "")
    info <- metricinfo(object)[[1]]
    cat("Arguments:\n")
    print(info$arguments)
    cat("\nTypes:\n")
    print(info$response_types)
  }
)


setMethod("show", "MLModel",
  function(object) {
    show_title(object)
    info <- modelinfo(object)[[1]]
    cat("\n",
        "Model name: ", object@name, "\n",
        "Label: ", info$label, "\n",
        "Packages: ", toString(info$packages), "\n",
        "Response types: ", toString(info$response_types), "\n",
        "Tuning grid: ", info$grid, "\n",
        "Variable importance: ", info$varimp, "\n\n",
        "Parameters:\n",
        sep = "")
    print(object@params)
    if (!length(object@params)) cat("\n")
    invisible()
  }
)


setMethod("show", "MLModelFit",
  function(object) {
    show(unMLModelFit(object))
  }
)


setMethod("show", "MLModelFunction",
  function(object) {
    show_title(object)
    info_list <- modelinfo(object)
    info <- info_list[[1]]
    cat("\n",
        "Model name: ", names(info_list), "\n",
        "Label: ", info$label, "\n",
        "Packages: ", toString(info$packages), "\n",
        "Response types: ", toString(info$response_types), "\n",
        "Tuning grid: ", info$grid, "\n",
        "Variable importance: ", info$varimp, "\n\n",
        "Arguments:\n",
        sep = "")
    print(info$arguments)
    cat("\n")
  }
)


setMethod("show", "MLModelTune",
  function(object) {
    callNextMethod(object)
    selected <- object@selected
    if (length(object@tune_grid)) {
      cat("Grid (selected = ", selected$index, "):\n", sep = "")
      print(object@tune_grid)
      cat("\n")
    }
    print(object@performance)
    if (!is.na(dim(object@performance)[3])) {
      model_names <- dimnames(object@performance)[[3]]
      cat("Selected model:", model_names[selected$index], "\n")
    }
    cat(names(selected$value), "value:", selected$value, "\n\n")
  }
)


setMethod("show", "ConfusionMatrix",
  function(object) {
    show_title(object)
    print(object@.Data)
  }
)


setMethod("show", "ConfusionSummary",
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


setMethod("show", "Curves",
  function(object){
    show_title(object)
    cat("\n",
        "Metrics: ",
        "x = ", object@metrics$x@label, ", ",
        "y = ", object@metrics$y@label, "\n\n",
        sep = "")
    print(as.data.frame(object))
  }
)


setMethod("show", "Performance",
  function(object) {
    show_title(object)
    cat("\n",
        "Metrics: ", toString(dimnames(object)[[2]]), "\n",
        sep = "")
    if (length(dim(object)) > 2) {
      cat("Models:", toString(dimnames(object)[[3]]), "\n")
    }
    cat("\n")
  }
)


setMethod("show", "PerformanceDiffTest",
  function(object) {
    show_title(object)
    cat("\n",
        "Upper diagonal: mean differences (row - column)\n",
        "Lower diagonal: p-values\n",
        "P-value adjustment method: ", object@adjust, "\n\n",
        sep = "")
    print(object@.Data)
  }
)


setMethod("show", "RecipeGrid",
  function(object) {
    show_title(object)
    print(as.data.frame(object))
  }
)


setMethod("show", "Resamples",
  function(object) {
    show_title(object)
    cat("\n",
        "Models: ", toString(levels(object$Model)), "\n",
        sep = "")
    if (isTRUE(nzchar(object@strata))) {
      cat("Stratification variable:", object@strata, "\n")
    }
    cat("\n")
    show(object@control)
    invisible()
  }
)


setMethod("show", "TunedRecipe",
  function(object) {
    show_title(object)
    cat("\n")
    print(as(object, "ModelRecipe"))
    cat("\nGrid:\n\n")
    print(object@grid)
  }
)


setMethod("show", "VarImp",
  function(object) {
    show_title(object)
    print(as.data.frame(object))
  }
)


show_title <- function(x, ...) {
  UseMethod("show_title")
}


show_title.default <- function(x, ...) {
  show_title(class(x)[1])
}


show_title.character <- function(x, ...) {
  cat("Object of class \"", x, "\"\n", sep = "")
}
