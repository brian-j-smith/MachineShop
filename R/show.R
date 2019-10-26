#' Print MachineShop Objects
#' 
#' Print methods for objects defined in the \pkg{MachineShop} package.
#'  
#' @name print
#' @rdname print-methods
#' 
#' @param x object to print.
#' @param n integer number of models or data frame rows to show.
#' @param ... arguments passed to other methods.
#' 
NULL


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


#' @rdname print-methods
#' 
print.MLModelTune <- function(x, n = MachineShop::settings("max.print"), ...) {
  NextMethod()
  selected <- x@selected
  if (length(x@tune_grid)) {
    cat("\nGrid (selected = ", selected$index, "):\n", sep = "")
    print_items(x@tune_grid, n = n)
    cat("\n")
  }
  print(x@performance, n = n)
  cat("\n")
  if (!is.na(dim(x@performance)[3])) {
    model_names <- dimnames(x@performance)[[3]]
    cat("Selected model:", model_names[selected$index], "\n")
  }
  cat(names(selected$value), "value:", selected$value, "\n")
  invisible(x)
}


print.ModelRecipe <- function(x, ...) {
  show_title(x)
  cat("\n")
  NextMethod()
  invisible(x)
}


#' @rdname print-methods
#' 
print.Performance <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  dn <- dimnames(x)
  cat("\nMetrics:", toString(dn[[2]]), "\n")
  if (length(dn) > 2) {
    cat("Models: ")
    print_items(dn[[3]], n = n)
  }
  invisible(x)
}


#' @rdname print-methods
#' 
print.RecipeGrid <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  print(asS3(x), n = n)
  invisible(x)
}


#' @rdname print-methods
#' 
print.Resamples <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  cat("\nModels: ")
  print_items(levels(x$Model), n = n)
  if (isTRUE(nzchar(x@strata))) {
    cat("Stratification variable:", x@strata, "\n")
  }
  cat("\n")
  show(x@control)
  invisible(x)
}


#' @rdname print-methods
#' 
print.SurvMatrix <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  print_items(as.data.frame(as(x, "matrix")), n = n)
  cat("Attribute \"times\":\n")
  print(time(x))
  invisible(x)
}


#' @rdname print-methods
#' 
print.TunedRecipe <- function(x, n = MachineShop::settings("max.print"), ...) {
  NextMethod()
  cat("\nGrid:\n\n")
  print_items(x@grid, n = n)
  invisible(x)
}


#' @rdname print-methods
#' 
print.VarImp <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  print_items(as(x, "data.frame"), n = n)
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


setMethod("show", "MLCrossValidationControl",
  function(object) {
    cat("Folds: ", object@folds, "\n",
        "Repeats: ", object@repeats, "\n",
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
        sep = "")
    callNextMethod()
    invisible()
  }
)


setMethod("show", "MLCVOptimismControl",
  function(object) {
    show_title("MLControl")
    cat("\n",
        "Name: CVOptimismControl\n",
        "Label: Optimism-Corrected K-Fold Cross-Validation\n",
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
    print(object)
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


setMethod("show", "ModelRecipe",
  function(object) {
    print(object)
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
    print(object)
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
    print(object)
    invisible()
  }
)


setMethod("show", "TunedRecipe",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "VarImp",
  function(object) {
    print(object)
    invisible()
  }
)


#################### Print Utility Functions ####################


print_items <- function(x, ...) {
  UseMethod("print_items")
}


print_items.default <- function(x, ...) {
  print(x)
}


print_items.character <- function(x, n, ...) {
  diff <- length(x) - n
  str <- if (diff > 0) {
    paste0(toString(head(x, n)), "... with ", diff, " more")
  } else {
    toString(x)
  }
  cat(str, "\n")
}


print_items.data.frame <- function(x, n, ...) {
  diff <- nrow(x) - n
  if (diff > 0) {
    print(head(x, n))
    cat("... with ", diff, " more row", if (diff > 1) "s", "\n", sep ="")
  } else {
    print(x)
  }
}


print_items.tbl <- function(x, n, ...) {
  print(x, n = n)
}


show_title <- function(x, ...) {
  UseMethod("show_title")
}


show_title.default <- function(x, ...) {
  show_title(class(x)[1])
}


show_title.character <- function(x, ...) {
  cat("Object of class \"", x, "\"\n", sep = "")
}
