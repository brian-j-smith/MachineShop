print.MLModel <- function(x, ...) {
  show_title(x)
  info <- modelinfo(x)[[1]]
  cat("\n",
      "Model name: ", x@name, "\n",
      "Label: ", info$label, "\n",
      "Packages: ", toString(info$packages), "\n",
      "Response types: ", toString(info$response_types), "\n",
      "Tuning grid: ", info$grid, "\n",
      "Variable importance: ", info$varimp, "\n\n",
      "Parameters:\n",
      sep = "")
  cat(str(x@params))
  invisible(x)
}


print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x), ...)
  invisible(x)
}


print.MLModelTune <- function(x, ...) {
  NextMethod()
  selected <- x@selected
  if (length(x@tune_grid)) {
    cat("\nGrid (selected = ", selected$index, "):\n", sep = "")
    print(x@tune_grid, ...)
    cat("\n")
  }
  print(x@performance)
  cat("\n")
  if (!is.na(dim(x@performance)[3])) {
    model_names <- dimnames(x@performance)[[3]]
    cat("Selected model:", model_names[selected$index], "\n")
  }
  cat(names(selected$value), "value:", selected$value, "\n")
  invisible(x)
}


print.RecipeGrid <- function(x, ...) {
  show_title(x)
  print(asS3(x), ...)
  invisible(x)
}


print.Resamples <- function(x, ...) {
  show(x)
  invisible(x)
}


print.SurvMatrix <- function(x, ...) {
  show_title(x)
  print(as(x, "matrix"))
  cat("Attribute \"times\":\n")
  print(time(x))
  invisible(x)
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
    cat("Seed:", object@seed, "\n")
    invisible()
  }
)


setMethod("show", "MLBootstrapControl",
  function(object) {
    cat("Samples:", object@samples, "\n")
    callNextMethod()
    invisible()
  }
)


setMethod("show", "MLBootControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: BootControl\n",
        "Label: Bootstrap Resampling\n",
        sep = "")
    callNextMethod()
    invisible()
  }
)


setMethod("show", "MLBootOptimismControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: BootOptimismControl\n",
        "Label: Optimism-Corrected Bootstrap Resampling\n",
        sep = "")
    callNextMethod()
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
    callNextMethod()
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
    callNextMethod()
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
    callNextMethod()
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
    callNextMethod()
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
    invisible()
  }
)


setMethod("show", "MLModel",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "MLModelFit",
  function(object) {
    show(unMLModelFit(object))
    invisible()
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
    invisible()
  }
)


setMethod("show", "MLModelList",
  function(object) {
    show_title(object)
    cat("\n")
    print(unclass(object))
    invisible()
  }
)


setMethod("show", "MLModelTune",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "Calibration",
  function(object) {
    show_title(object)
    print(as(object, "data.frame"))
    invisible()
  }
)


setMethod("show", "ConfusionMatrix",
  function(object) {
    show_title(object)
    print(as(object, "table"))
    invisible()
  }
)


setMethod("show", "ConfusionSummary",
  function(object) {
    total <- object@total
    acc <- object@accuracy
    cat("Number of responses: ", total, "\n",
        "Accuracy (SE): ", acc, " (", sqrt(acc * (1 - acc) / total), ")\n",
        "Majority class: ", object@majority, "\n",
        "Kappa: ", object@kappa2, "\n\n",
        sep = "")
    print(as(object, "matrix"))
    invisible()
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
    print(as(object, "data.frame"))
    invisible()
  }
)


setMethod("show", "Performance",
  function(object) {
    show_title(object)
    dn <- dimnames(object)
    cat("\n",
        "Metrics: ", toString(dn[[2]], width = show_width(offset = 9)), "\n",
        sep = "")
    if (length(dim(object)) > 2) {
      cat("Models:", toString(dn[[3]], width = show_width(offset = 8)), "\n")
    }
    invisible()
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
    print(as(object, "array"))
    invisible()
  }
)


setMethod("show", "RecipeGrid",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "Resamples",
  function(object) {
    show_title(object)
    cat("\n",
        "Models: ",
        toString(levels(object$Model), width = show_width(offset = 8)), "\n",
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
    invisible()
  }
)


setMethod("show", "VarImp",
  function(object) {
    show_title(object)
    print(as(object, "data.frame"))
    invisible()
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


show_width <- function(offset = 0, min = max(80 - offset, 60)) {
  max(getOption("width") - offset, min)
}
