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
print.BinomialVariate <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x)
  print_items(as(x, "matrix"), n = n)
  invisible(x)
}


#' @rdname print-methods
#'
print.Calibration <- function(x, n = MachineShop::settings("print_max"), ...) {
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


print.ConfusionList <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x)
  cat("\n")
  NextMethod()
}


print.ConfusionMatrix <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
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


print.ConfusionSummary <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  total <- x@total
  acc <- x@accuracy
  cat("Number of responses: ", total, "\n",
      "Accuracy (SE): ", acc, " (", sqrt(acc * (1 - acc) / total), ")\n",
      "Majority class: ", x@majority, "\n",
      "Kappa: ", x@kappa2, "\n\n",
      sep = "")
  print(as(x, "matrix"))
  invisible(x)
}


setMethod("show", "ConfusionSummary",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.DiscreteVariate <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x)
  print_items(as(x, "numeric"), n = n)
  cat("Range: ", x@min, ", ", x@max, "\n", sep = "")
  invisible(x)
}


setMethod("show", "DiscreteVariate",
  function(object) {
    print(object)
    invisible()
  }
)


print.Grid <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  cat("\n",
      label_items("Grid size", x@size, add_names = TRUE), "\n",
      "Random sample: ", x@random, "\n",
      sep = ""
  )
  invisible()
}


setMethod("show", "Grid",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.ListOf <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_items(as(x, "listof"), n = n)
  invisible(x)
}


setMethod("show", "ListOf",
  function(object) {
    print(object)
    invisible()
  }
)


print.MLControl <- function(x, n = MachineShop::settings("print_max"), ...) {
  show(x)
  invisible(x)
}


setMethod("show", "MLControl",
  function(object) {
    cat("Stratification parameters\n",
        "  Breaks: ", object@strata_breaks, "\n",
        "  Unique numeric threshold: ", object@strata_nunique, "\n",
        "  Minimum proportion: ", object@strata_prop, "\n",
        "  Minimum size: ", object@strata_size, "\n",
        sep = "")
    opts <- c("times" = "Survival times",
              "method" = "Method",
              "distr" = "Distribution")
    for (name in names(opts)) {
      x <- slot(object, name)
      if (length(x)) cat(opts[name], ": ", toString(x), "\n", sep = "")
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


print.MLMetric <- function(x, n = MachineShop::settings("print_max"), ...) {
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
print.MLModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nParameters:\n")
  cat(str(x@params))
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n)
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


print.MLModelFunction <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
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
print.ModelFrame <- function(x, n = MachineShop::settings("print_max"), ...) {
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


print.ModelRecipe <- function(x, n = MachineShop::settings("print_max"), ...) {
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


print.ModelTerms <- function(x, n = MachineShop::settings("print_max"), ...) {
  print(formula(x))
}


#' @rdname print-methods
#'
print.ModeledInput <- function(x, n = MachineShop::settings("print_max"), ...) {
  NextMethod()
  cat("\n")
  print(x@model, n = n)
  invisible(x)
}


print.ModeledTerms <- function(x, n = MachineShop::settings("print_max"), ...) {
  print(formula(x))
  cat("\n")
  print(x@model, n)
  invisible(x)
}


print.ParameterGrid <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  NextMethod()
  cat("\n")
  print(as(x, "parameters"))
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
print.Performance <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  dn <- dimnames(x)
  cat("\nMetrics:", toString(dn[[2]]), "\n")
  if (length(dn) > 2) {
    print_items(dn[[3]], n = n, prefix = "Models: ", exdent = 2)
  }
  invisible(x)
}


#' @rdname print-methods
#'
print.PerformanceCurve <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x)
  cat("\n",
      "Metrics: ",
      "x = ", x@metrics$x@label, ", ",
      "y = ", x@metrics$y@label, "\n\n",
      sep = "")
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setMethod("show", "PerformanceCurve",
  function(object){
    print(object)
    invisible()
  }
)


print.PerformanceDiffTest <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x)
  cat("\n",
      "Upper diagonal: mean differences (Model1 - Model2)\n",
      "Lower diagonal: p-values\n",
      "P-value adjustment method: ", x@adjust, "\n\n",
      sep = "")
  print(as(x, "array"))
  invisible(x)
}


#' @rdname print-methods
#'
print.RecipeGrid <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  print_items(as(x, "tbl_df"), n = n)
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
print.Resamples <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  print_items(levels(x$Model), n = n, prefix = "\nModels: ", exdent = 2)
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
print.SelectedInput <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  NextMethod()
  cat("\nModel inputs:\n\n")
  print(x@inputs, n = n)
  print(x@params$control)
  invisible(x)
}


print.SelectedModel <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x)
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nSelection parameters:\n\n")
  print(x@params$models, n = n)
  print(x@params$control)
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n)
  }
  invisible(x)
}


print.StackedModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nParameters:\n")
  cat(str(x@params[setdiff(names(x@params), c("base_learners", "control"))]))
  cat("\nBase learners:\n\n")
  print(x@params$base_learners, n = n)
  print(x@params$control)
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n)
  }
  invisible(x)
}


print.SuperModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nParameters:\n")
  subset <- !(names(x@params) %in% c("base_learners", "control", "model"))
  cat(str(x@params[subset]))
  cat("\nSuper learner:\n\n")
  print(x@params$model)
  cat("\nBase learners:\n\n")
  print(x@params$base_learners, n = n)
  print(x@params$control)
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n)
  }
  invisible(x)
}


format.SurvMatrix <- function(x, ...) {
  format(as(x, "matrix"), ...)
}


#' @rdname print-methods
#'
print.SurvMatrix <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  print_items(as(x, "matrix"), n = n)
  cat("Times:\n")
  print(x@times)
  cat("Distribution:", x@distr, "\n")
  invisible(x)
}


setMethod("show", "SurvMatrix",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.SurvMeans <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  print_items(as(x, "numeric"), n = n)
  cat("Distribution:", x@distr, "\n")
  invisible(x)
}


setMethod("show", "SurvMeans",
  function(object) {
    print(object)
    invisible()
  }
)


print.TabularArray <- function(x, n = MachineShop::settings("print_max"), ...) {
  print(as(x, "array"))
  invisible(x)
}


setMethod("show", "TabularArray",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.TrainStep <- function(x, n = MachineShop::settings("print_max"), ...) {
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


setMethod("show", "TrainStep",
  function(object) {
    print(object)
    invisible()
  }
)


#' @rdname print-methods
#'
print.TunedInput <- function(x, n = MachineShop::settings("print_max"), ...) {
  NextMethod()
  cat("\n")
  print_items(x@grid, n = n)
  cat("\n")
  print(x@params$control)
  invisible(x)
}


print.TunedModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x)
  trained <- is_trained(x)
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
    print(x@train_steps, n = n)
  }
  invisible(x)
}


#' @rdname print-methods
#'
print.VarImp <- function(x, n = MachineShop::settings("print_max"), ...) {
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


format_len <- function(x) format(x, big.mark = ",")


print_items <- function(x, ...) {
  UseMethod("print_items")
}


print_items.default <- function(x, ...) {
  print(x)
}


print_items.character <- function(x, n = Inf, prefix = "", exdent = 0, ...) {
  diff <- length(x) - n
  str <- if (diff > 0) {
    paste0(toString(head(x, n)), "... with ", format_len(diff), " more")
  } else {
    toString(x)
  }
  writeLines(strwrap(str, initial = prefix, width = getOption("width"),
                     exdent = exdent))
}


print_items.data.frame <- function(x, ...) {
  print_items.matrix(format(x), ...)
}


print_items.list <- function(x, n = Inf, n_extra = 10 * n, ...) {
  diff <- length(x) - n
  if (diff > 0) {
    print(head(x, n), max = n)
    label <- paste("... with", format_len(diff), "more element")
    print_items(label_items(label, tail(names(x), diff), n_extra))
    cat("\n")
  } else {
    print(x, max = n)
  }
}


print_items.listof <- function(x, n = Inf, n_extra = 10 * n, ...) {
  inds <- head(seq_along(x), n)
  x_names <- names(x)
  if (is.null(x_names)) x_names <- paste("Component", seq_along(x))
  vsep <- strrep("-", 0.75 * getOption("width"))
  for (i in inds) {
    cat(x_names[i], ":\n")
    print(x[[i]], n = n, na.print = NULL)
    if (i != length(x)) cat(vsep, "\n") else cat("\n")
  }
  n_more <- max(length(x) - n, 0)
  if (n_more) {
    label <- paste("... with", format_len(n_more), "more element")
    print_items(label_items(label, tail(x_names, n_more), n_extra))
    cat("\n")
  }
}


print_items.matrix <- function(x, n = Inf, n_extra = 10 * n, ...) {
  row_inds <- head(seq_len(nrow(x)), n)
  col_inds <- head(seq_len(ncol(x)), n)
  num_items <- length(row_inds) * length(col_inds)
  if (num_items) {
    print(x[row_inds, col_inds, drop = FALSE], max = num_items)
  } else {
    cat("<", format_len(nrow(x)), " x ", format_len(ncol(x)), " ", class(x)[1],
        ">\n", sep = "")
  }
  diff_rows <- nrow(x) - length(row_inds)
  diff_cols <- ncol(x) - length(col_inds)
  cols <- tail(colnames(x), diff_cols)
  if (diff_rows) {
    str <- paste0("... with ", format_len(diff_rows), " more row",
                  if (diff_rows > 1) "s")
    if (diff_cols) {
      str <- paste(str, "and", format_len(diff_cols),
                   label_items("column", cols, n_extra))
    }
    print_items(str)
  } else if (diff_cols) {
    label <- paste("... with", format_len(diff_cols), "more column")
    print_items(label_items(label, cols, n_extra))
  }
}


print_items.numeric <- function(x, n = Inf, ...) {
  diff <- length(x) - n
  if (diff > 0) {
    print(head(x, n), max = n)
    cat("... with ", format_len(diff), " more value", if (diff > 1) "s", "\n",
        sep = "")
  } else {
    print(x, max = n)
  }
}


print_items.tbl <- function(x, n = Inf, n_extra = 10 * n, ...) {
  print(x, n = n, n_extra = n_extra)
}


print_modelinfo <- function(x, trained = FALSE) {
  info_list <- modelinfo(x)
  info <- info_list[[1]]
  cat("\n",
      "Model name: ", names(info_list), "\n",
      "Label: ", if (trained) "Trained ", info$label, "\n",
      sep = "")
  print_items(label_items("Package", info$packages), exdent = 2)
  print_items(label_items("Response type", info$response_types), exdent = 2)
  cat("Tuning grid: ", info$grid, "\n",
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
