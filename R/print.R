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
  print_title(x)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setMethod("show", "Calibration",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.ConfusionList <- function(x, n = MachineShop::settings("max.print"),
                                ...) {
  print_title(x)
  cat("\n")
  x[] <- lapply(x, as, Class = "table")
  NextMethod()
}


print.ConfusionMatrix <- function(x, ...) {
  print_title(x)
  print(as(x, "table"))
  invisible(x)
}


setMethod("show", "ConfusionMatrix",
  function(object) {
    print(object)
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


#' @rdname print-methods
#'
print.Curves <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  cat("\n",
      "Metrics: ",
      "x = ", x@metrics$x@label, ", ",
      "y = ", x@metrics$y@label, "\n\n",
      sep = "")
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setMethod("show", "Curves",
  function(object){
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.BinomialVariate <- function(x, n = MachineShop::settings("max.print"),
                                  ...) {
  print_title(x)
  print_items(as(x, "matrix"), n = n)
  invisible(x)
}


setMethod("show", "DiscreteVariate",
  function(object) {
    print_title(object)
    print(as(object, "numeric"))
    cat("Range: ", object@min, ", ", object@max, "\n", sep = "")
    invisible()
  }
)


setMethod("show", "Grid",
  function(object) {
    print_title(object)
    cat("\n",
        "Length: ", object@length, "\n",
        "Random sample: ", object@random, "\n",
        sep = ""
    )
    invisible()
  }
)


#' @rdname print-methods
#'
print.ListOf <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_items(as(x, "listof"), n = n)
  invisible(x)
}


setMethod("show", "ListOf",
  function(object) {
    print(object)
    invisible()
  }
)


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
    print_title("MLControl")
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
    print_title("MLControl")
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
    print_title("MLControl")
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
    print_title("MLControl")
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
    print_title("MLControl")
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
    print_title("MLControl")
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
    print_title("MLControl")
    cat("\n",
        "Name: TrainControl\n",
        "Label: Training Resubstitution\n",
        sep = "")
    callNextMethod()
    invisible()
  }
)


print.MLMetric <- function(x, ...) {
  print_title(x)
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


setMethod("show", "MLMetric",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.MLModel <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  print_modelinfo(x)
  cat("\nParameters:\n")
  cat(str(x@params))
  if (is.trained(x)) {
    cat("\n")
    print_items(x@trainbits, n = n)
  }
  invisible(x)
}


setMethod("show", "MLModel",
  function(object) {
    print(object)
    invisible()
  }
)


print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x), ...)
  invisible(x)
}


setMethod("show", "MLModelFit",
  function(object) {
    print(object)
    invisible()
  }
)


print.MLModelFunction <- function(x, ...) {
  print_title(x)
  print_modelinfo(x)
  cat("\nArguments:\n")
  print(args(x))
  invisible(x)
}


setMethod("show", "MLModelFunction",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.ModelFrame <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setMethod("show", "ModelFrame",
  function(object) {
    print(object)
    invisible()
  }
)


print.ModelRecipe <- function(x, ...) {
  print_title(x)
  cat("\n")
  NextMethod()
  invisible(x)
}


setMethod("show", "ModelRecipe",
  function(object) {
    print(object)
    invisible()
  }
)


print.ParameterGrid <- function(x, ...) {
  print_title(x)
  print(as(asS3(x), "tbl_df"))
  if (x@random) {
    cat("Random sample:", x@random, "\n")
  } else {
    cat(label_items("Length", x@length), "\n")
  }
  invisible(x)
}


setMethod("show", "ParameterGrid",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.Performance <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  dn <- dimnames(x)
  cat("\nMetrics:", toString(dn[[2]]), "\n")
  if (length(dn) > 2) {
    cat("Models: ")
    print_items(dn[[3]], n = n)
  }
  invisible(x)
}


setMethod("show", "Performance",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "PerformanceDiffTest",
  function(object) {
    print_title(object)
    cat("\n",
        "Upper diagonal: mean differences (Model1 - Model2)\n",
        "Lower diagonal: p-values\n",
        "P-value adjustment method: ", object@adjust, "\n\n",
        sep = "")
    print(as(object, "array"))
    invisible()
  }
)


#' @rdname print-methods
#'
print.RecipeGrid <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  print_items(as(asS3(x), "tbl_df"), n = n)
  invisible(x)
}


setMethod("show", "RecipeGrid",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.Resamples <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  cat("\nModels: ")
  print_items(levels(x$Model), n = n)
  if (isTRUE(nzchar(x@strata))) {
    cat("Stratification variable:", x@strata, "\n")
  }
  cat("\n")
  print(x@control)
  invisible(x)
}


setMethod("show", "Resamples",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.SelectedInput <- function(x, n = MachineShop::settings("max.print"),
                                ...) {
  NextMethod()
  cat("\nModel inputs:\n\n")
  print_items(x@inputs, n = n)
  print(x@params$control)
  invisible(x)
}


#' @rdname print-methods
#'
print.SelectedModel <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  trained <- is.trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nSelection parameters:\n\n")
  print_items(x@params$models, n = n)
  print(x@params$control)
  if (trained) {
    cat("\n")
    print_items(x@trainbits, n = n)
  }
  invisible(x)
}


#' @rdname print-methods
#'
print.StackedModel <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  print_modelinfo(x)
  cat("\nParameters:\n")
  cat(str(x@params[setdiff(names(x@params), c("base_learners", "control"))]))
  cat("\nBase learners:\n\n")
  print_items(x@params$base_learners, n = n)
  print(x@params$control)
  invisible(x)
}


#' @rdname print-methods
#'
print.SuperModel <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  print_modelinfo(x)
  cat("\nParameters:\n")
  subset <- !(names(x@params) %in% c("base_learners", "control", "model"))
  cat(str(x@params[subset]))
  cat("\nSuper learner:\n\n")
  print(x@params$model)
  cat("\nBase learners:\n\n")
  print_items(x@params$base_learners, n = n)
  print(x@params$control)
  invisible(x)
}


format.SurvMatrix <- function(x, ...) {
  format(as(x, "matrix"), ...)
}


#' @rdname print-methods
#'
print.SurvMatrix <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  print_items(as(x, "matrix"), n = n)
  cat("Times:\n")
  print(x@times)
  invisible(x)
}


setMethod("show", "SurvMatrix",
  function(object) {
    print(object)
    invisible()
  }
)


setMethod("show", "TabularArray",
  function(object) {
    print(as(object, "array"))
    invisible()
  }
)


print.Terms <- function(x, ...) {
  print(formula(x))
}


#' @rdname print-methods
#'
print.TrainBits <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  if (length(x@grid)) {
    cat("\nGrid (selected = ", x@selected, "):\n", sep = "")
    print_items(x@grid, n = n)
  }
  cat("\n")
  print(x@performance, n = n)
  cat("\n")
  if (length(x@values) > 1) {
    cat("Selected model:", names(x@values)[x@selected], "\n")
  }
  cat(names(x@selected), "value:", x@values[x@selected], "\n")
  invisible(x)
}


setMethod("show", "TrainBits",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.TunedInput <- function(x, n = MachineShop::settings("max.print"), ...) {
  NextMethod()
  cat("\n")
  print_items(x@grid, n = n)
  cat("\n")
  print(x@params$control)
  invisible(x)
}


#' @rdname print-methods
#'
print.TunedModel <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  trained <- is.trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nTuning parameters:\n\n")
  print(x@params$model)
  cat("\n")
  grid <- x@params$grid
  if (!isS4(grid)) cat("Grid:\n")
  print_items(grid, n = n)
  cat("\n")
  print(x@params$control)
  if (trained) {
    cat("\n")
    print_items(x@trainbits, n = n)
  }
  invisible(x)
}


#' @rdname print-methods
#'
print.VarImp <- function(x, n = MachineShop::settings("max.print"), ...) {
  print_title(x)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


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


print_items.data.frame <- function(x, ...) {
  print_items.matrix(x, ...)
}


print_items.list <- function(x, n, ...) {
  diff <- length(x) - n
  if (diff > 0) {
    print(head(x, n), max = n)
    cat("... with ", diff, " more element", if (diff > 1) "s", ": ",
        toString(tail(names(x), diff)), "\n\n", sep = "")
  } else {
    print(x, max = n)
  }
}


print_items.listof <- function(x, ...) {
  print_items.list(x, ...)
}


print_items.matrix <- function(x, n, ...) {
  diff <- nrow(x) - n
  max_items <- n * ncol(x)
  if (diff > 0) {
    print(head(x, n), max = max_items)
    cat("... with ", diff, " more row", if (diff > 1) "s", "\n", sep ="")
  } else {
    print(x, max = max_items)
  }
}


print_items.tbl <- function(x, n, ...) {
  print(x, n = n)
}


print_modelinfo <- function(x, trained = FALSE) {
  info_list <- modelinfo(x)
  info <- info_list[[1]]
  cat("\n",
      "Model name: ", names(info_list), "\n",
      "Label: ", if (trained) "Trained ", info$label, "\n",
      label_items("Package", info$packages), "\n",
      label_items("Response type", info$response_types), "\n",
      "Tuning grid: ", info$grid, "\n",
      "Variable importance: ", info$varimp, "\n",
      sep = "")
}


print_title <- function(x, ...) {
  UseMethod("print_title")
}


print_title.default <- function(x, ...) {
  print_title(class(x)[1])
}


print_title.character <- function(x, ...) {
  cat("Object of class \"", x, "\"\n", sep = "")
}
