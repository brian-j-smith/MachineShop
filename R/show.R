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


#' @rdname print-methods
#' 
print.Calibration <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


#' @rdname print-methods
#' 
print.Curves <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  cat("\n",
      "Metrics: ",
      "x = ", x@metrics$x@label, ", ",
      "y = ", x@metrics$y@label, "\n\n",
      sep = "")
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


print.MLMetric <- function(x, ...) {
  show_title(x)
  cat("\n",
      "Metric name: ", x@name, "\n",
      "Label: ", x@label, "\n",
      "Maximize: ", x@maximize, "\n\n",
      sep = "")
  info <- metricinfo(x)[[1]]
  cat("Arguments:\n")
  print(info$arguments)
  cat("\nTypes:\n")
  print(info$response_types)
  invisible(x)
}


#' @rdname print-methods
#' 
print.MLModel <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  info <- modelinfo(x)[[1]]
  is_tuned <- !is.null(x@tune)
  cat("\n",
      "Model name: ", x@name, "\n",
      "Label: ", if (is_tuned) "Tuned ", info$label, "\n",
      "Packages: ", toString(info$packages), "\n",
      "Response types: ", toString(info$response_types), "\n",
      "Tuning grid: ", info$grid, "\n",
      "Variable importance: ", info$varimp, "\n\n",
      sep = "")
  if (is(x, "SelectedModel")) {
    cat("Selection Parameters:\n\n")
    print_items(x@params$models, n = n)
    show(x@params$control)
  } else if (is(x, "TunedModel")) {
    cat("Tuning Parameters:\n\n")
    print(x@params$model)
    cat("\n")
    grid <- x@params$grid
    if (length(dim(grid)) == 2) {
      cat("Grid:\n")
      print_items(grid, n = n)
    } else {
      print(grid)
    }
    cat("\n")
    show(x@params$control)
  } else {
    cat("Parameters:\n")
    cat(str(x@params))
  }
  if (is_tuned) {
    cat("\n")
    print(x@tune, n = n)
  }
  invisible(x)
}


print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x), ...)
  invisible(x)
}


print.MLModelFunction <- function(x, ...) {
  show_title(x)
  info_list <- modelinfo(x)
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
  invisible(x)
}


#' @rdname print-methods
#' 
print.MLTune <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  if (length(x@grid)) {
    cat("\nGrid (selected = ", x@selected, "):\n", sep = "")
    print_items(x@grid, n = n)
    cat("\n")
  }
  print(x@performance, n = n)
  cat("\n")
  if (length(x@values) > 1) {
    cat("Selected model:", names(x@values)[x@selected], "\n")
  }
  cat(names(x@selected), "value:", x@values[x@selected], "\n")
  invisible(x)
}


#' @rdname print-methods
#' 
print.ModelFrame <- function(x, n = MachineShop::settings("max.print"), ...) {
  show_title(x)
  print_items(as(x, "data.frame"), n = n)
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
  cat("\n")
  print_items(x@grid, n = n)
  cat("\n")
  show(x@params$control)
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
    print(object)
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
    print(object)
    invisible()
  }
)


setMethod("show", "MLTune",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "ModelRecipe",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "Calibration",
  function(object) {
    print(object)
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
    print(object)
    invisible()
  }
)


setMethod("show", "Grid",
  function(object) {
    show_title(object)
    cat("\n",
        "Length: ", object@length, "\n",
        "Random sample: ", object@random, "\n",
        sep = ""
    )
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


setMethod("show", "SelectedRecipe",
  function(object) {
    show_title(object)
    cat("\n")
    print(object@recipes)
    show(object@params$control)
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


print_items.list <- function(x, n, ...) {
  diff <- length(x) - n
  if (diff > 0) {
    print(head(x, n))
    cat("... with ", diff, " more element", if (diff > 1) "s", ": ",
        toString(tail(names(x), diff)), "\n\n", sep = "")
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
