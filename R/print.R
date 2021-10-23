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


setShowDefault <- function(signature) {
  setMethod("show", signature, function(object) {
    print(object)
    invisible()
  })
}


#' @rdname print-methods
#'
print.BinomialVariate <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x, ...)
  print_items(as(x, "matrix"), n = n)
  invisible(x)
}


#' @rdname print-methods
#'
print.Calibration <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setShowDefault("Calibration")


print.ConfusionList <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  level <- title_level(print_title(x, ...)) + 1
  cat("\n")
  NextMethod(level = level)
}


print.ConfusionMatrix <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x, ...)
  print(as(x, "table"))
  invisible(x)
}


setShowDefault("ConfusionMatrix")


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


setShowDefault("ConfusionSummary")


#' @rdname print-methods
#'
print.DiscreteVariate <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x, ...)
  print_items(as(x, "numeric"), n = n)
  cat("Range: ", x@min, ", ", x@max, "\n", sep = "")
  invisible(x)
}


setShowDefault("DiscreteVariate")


print.Grid <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
  cat("\n",
      label_items("Grid size", x@size, add_names = TRUE), "\n",
      "Random sample: ", x@random, "\n",
      sep = ""
  )
  invisible()
}


setShowDefault("Grid")


#' @rdname print-methods
#'
print.ListOf <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_items(x, n = n, ...)
  invisible(x)
}


setShowDefault("ListOf")


print.MLControl <- function(x, ...) {
  print_params <- function(x, labels, title) {
    is_set <- !map_logi(is.null, x[names(labels)])
    if (any(is_set)) {
      cat(title, "\n", sep = "")
      for (name in names(labels)[is_set]) {
        cat("  ", labels[name], ": ", toString(x[[name]]), "\n", sep = "")
      }
    }
  }
  labels <- c("times" = "Survival times",
              "distr" = "Distribution",
              "method" = "Method")
  print_params(x@predict, labels, "Prediction parameters")
  labels <- c("breaks" = "Breaks",
              "nunique" = "Unique numeric threshold",
              "prop" = "Minimum proportion",
              "size" = "Minimum size")
  print_params(x@strata, labels, "Stratification parameters")
  monitor <- x@monitor[map_logi(isTRUE, x@monitor)]
  if (length(monitor)) cat("Monitoring:", toString(names(monitor)), "\n")
  cat("Seed:", x@seed, "\n")
  invisible(x)
}


setShowDefault("MLControl")


print.MLBootControl <- function(x, ...) {
  print_controltitle(x, ...)
  cat("Samples: ", x@samples, "\n", sep = "")
  NextMethod()
  invisible(x)
}


print.MLCVControl <- function(x, ...) {
  print_controltitle(x, ...)
  cat("Folds: ", x@folds, "\n",
      "Repeats: ", x@repeats, "\n",
      sep = "")
  NextMethod()
  invisible(x)
}


print.MLOOBControl <- function(x, ...) {
  print_controltitle(x, ...)
  cat("Samples: ", x@samples, "\n", sep = "")
  NextMethod()
  invisible(x)
}


print.MLSplitControl <- function(x, ...) {
  print_controltitle(x, ...)
  cat("Training proportion: ", x@prop, "\n", sep = "")
  NextMethod()
  invisible(x)
}


print.MLTrainControl <- function(x, ...) {
  print_controltitle(x, ...)
  NextMethod()
  invisible(x)
}


print.MLMetric <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
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


setShowDefault("MLMetric")


#' @rdname print-methods
#'
print.MLModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- title_level(print_title(x, ...)) + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nParameters:\n")
  cat(str(x@params, give.attr = FALSE))
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n, level = level)
  }
  invisible(x)
}


setShowDefault("MLModel")


print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x), ...)
  invisible(x)
}


setShowDefault("MLModelFit")


print.MLModelFunction <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x, ...)
  print_modelinfo(x)
  cat("\nArguments:\n")
  print(args(x))
  invisible(x)
}


setShowDefault("MLModelFunction")


#' @rdname print-methods
#'
print.ModelFrame <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
  width <- getOption("width")
  lines <- strwrap(
    deparse1(formula(x)), initial = "\nModel formula: ", width = width,
    exdent = 2
  )
  nlines <- max(1, round(log(n)))
  if (length(lines) > nlines) {
    lines <- head(lines, nlines)
    lines[nlines] <- paste0(
      trimws(strtrim(lines[nlines], width - 3), "right"), "..."
    )
  }
  writeLines(lines)
  cat("\nData frame:\n")
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setShowDefault("ModelFrame")


print.ModelRecipe <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- title_level(print_title(x, ...)) + 1
  cat("\n")
  NextMethod(level = level)
  invisible(x)
}


setShowDefault("ModelRecipe")


print.ModelTerms <- function(x, n = MachineShop::settings("print_max"), ...) {
  print(formula(x))
}


#' @rdname print-methods
#'
print.ModeledInput <- function(x, n = MachineShop::settings("print_max"), ...) {
  NextMethod()
  level <- title_level(list(...)) + 1
  cat("\n")
  print(x@model, n = n, level = level)
  invisible(x)
}


print.ModeledTerms <- function(x, n = MachineShop::settings("print_max"), ...) {
  print(formula(x))
  level <- title_level(list(...)) + 1
  cat("\n")
  print(x@model, n = n, level = level)
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


setShowDefault("ParameterGrid")


#' @rdname print-methods
#'
print.Performance <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
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
  print_title(x, ...)
  cat("\n",
      "Metrics: ",
      "x = ", x@metrics$x@label, ", ",
      "y = ", x@metrics$y@label, "\n\n",
      sep = "")
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setShowDefault("PerformanceCurve")


print.PerformanceDiffTest <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  print_title(x, ...)
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
  print_title(x, ...)
  print_items(as(x, "tbl_df"), n = n)
  invisible(x)
}


setShowDefault("RecipeGrid")


#' @rdname print-methods
#'
print.Resamples <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- title_level(print_title(x, ...)) + 1
  print_items(levels(x$Model), n = n, prefix = "\nModels: ", exdent = 2)
  labels <- c(groups = "Grouping", strata = "Stratification")
  for (type in names(labels)) {
    comp <- x@case_comps[[type]]
    if (length(comp)) cat(labels[type], "variable:", names(comp), "\n")
  }
  cat("\n")
  print(x@control, level = level)
  invisible(x)
}


setShowDefault("Resamples")


#' @rdname print-methods
#'
print.SelectedInput <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  NextMethod()
  level <- title_level(list(...)) + 1
  cat("\nModel inputs:\n\n")
  print(x@inputs, n = n, level = level)
  print(x@params$control, level = level)
  invisible(x)
}


print.SelectedModel <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  level <- title_level(print_title(x, ...)) + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nSelection parameters:\n\n")
  print(x@params$models, n = n, level = level)
  print(x@params$control, level = level)
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n, level = level)
  }
  invisible(x)
}


print.StackedModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- title_level(print_title(x, ...)) + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nParameters:\n")
  subset <- setdiff(names(x@params), c("base_learners", "control"))
  cat(str(x@params[subset], give.attr = FALSE))
  cat("\nBase learners:\n\n")
  print(x@params$base_learners, n = n, level = level)
  print(x@params$control, level = level)
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n, level = level)
  }
  invisible(x)
}


print.SuperModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- title_level(print_title(x, ...)) + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nParameters:\n")
  subset <- setdiff(names(x@params), c("base_learners", "control", "model"))
  cat(str(x@params[subset], give.attr = FALSE))
  cat("\nSuper learner:\n\n")
  print(x@params$model, level = level)
  cat("\nBase learners:\n\n")
  print(x@params$base_learners, n = n, level = level)
  print(x@params$control, level = level)
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n, level = level)
  }
  invisible(x)
}


format.SurvMatrix <- function(x, ...) {
  format(as(x, "matrix"), ...)
}


#' @rdname print-methods
#'
print.SurvMatrix <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
  print_items(as(x, "matrix"), n = n)
  cat("Times:\n")
  print(x@times)
  cat("Distribution:", x@distr, "\n")
  invisible(x)
}


setShowDefault("SurvMatrix")


#' @rdname print-methods
#'
print.SurvTimes <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
  print_items(as(x, "numeric"), n = n)
  cat("Distribution:", x@distr, "\n")
  invisible(x)
}


setShowDefault("SurvTimes")


print.TabularArray <- function(x, n = MachineShop::settings("print_max"), ...) {
  print(as(x, "array"))
  invisible(x)
}


setShowDefault("TabularArray")


#' @rdname print-methods
#'
print.TrainStep <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- title_level(print_title(x, ...)) + 1
  if (length(x@grid)) {
    cat("\nGrid (selected = ", x@selected, "):\n", sep = "")
    print_items(x@grid, n = n)
  }
  cat("\n")
  print(x@performance, n = n, level = level)
  cat("\n")
  if (length(x@values) > 1) {
    cat("Selected model:", names(x@values)[x@selected], "\n")
  }
  cat(names(x@selected), "value:", x@values[x@selected], "\n")
  invisible(x)
}


setShowDefault("TrainStep")


#' @rdname print-methods
#'
print.TunedInput <- function(x, n = MachineShop::settings("print_max"), ...) {
  NextMethod()
  level <- title_level(list(...)) + 1
  cat("\n")
  grid <- x@grid
  if (isS4(grid)) {
    print(grid, level = level)
  } else {
    print_heading("Grid:\n", level = level)
    print_items(grid, n = n)
  }
  cat("\n")
  print(x@params$control, level = level)
  invisible(x)
}


print.TunedModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- title_level(print_title(x, ...)) + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained)
  cat("\nTuning parameters:\n\n")
  print(x@params$model, level = level)
  cat("\n")
  grid <- x@params$grid
  if (isS4(grid)) {
    print(grid, level = level)
  } else {
    print_heading("Grid:\n", level = level)
    print_items(grid, n = n)
  }
  cat("\n")
  print(x@params$control, level = level)
  if (trained) {
    cat("\n")
    print(x@train_steps, n = n, level = level)
  }
  invisible(x)
}


#' @rdname print-methods
#'
print.VarImp <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_title(x, ...)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setShowDefault("VarImp")


#################### Print Utility Functions ####################


format_len <- function(x) format(x, big.mark = ",")


print_controltitle <- function(x, ...) {
  print_title("MLControl", ...)
  cat("\n",
      "Name: ", x@name, "\n",
      "Label: ", x@label, "\n",
      sep = "")
}


print_default <- function(x, max = NULL, ...) {
  if (identical(max, Inf)) max <- .Machine$integer.max
  print(x, max = max, ...)
}


print_heading <- function(x, level = 0, ...) {
  prefix <- if (level) paste0(strrep("#", level), " ")
  cat(prefix, x, sep = "")
}


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
  writeLines(strwrap(
    str, initial = prefix, width = getOption("width"), exdent = exdent
  ))
}


print_items.data.frame <- function(x, ...) {
  print_items.matrix(format(x), ...)
}


print_items.list <- function(x, n = Inf, n_extra = 10 * n, ...) {
  diff <- length(x) - n
  if (diff > 0) {
    print_default(head(x, n), max = n)
    label <- paste("... with", format_len(diff), "more element")
    print_items(label_items(label, tail(names(x), diff), n_extra))
    cat("\n")
  } else {
    print_default(x, max = n)
  }
}


print_items.ListOf <- function(x, n = Inf, n_extra = 10 * n, level = 0, ...) {
  inds <- head(seq_along(x), n)
  x_names <- names(x)
  if (is.null(x_names)) x_names <- paste("Component", seq_along(x))
  vsep <- strrep("-", 0.75 * getOption("width"))
  for (i in inds) {
    cat(x_names[i], ":\n")
    print(x[[i]], n = n, level = level, na.print = NULL)
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
    print_default(x[row_inds, col_inds, drop = FALSE], max = num_items)
  } else {
    cat("<", format_len(nrow(x)), " x ", format_len(ncol(x)), " ", class1(x),
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
    print_default(head(x, n), max = n)
    cat("... with ", format_len(diff), " more value", if (diff > 1) "s", "\n",
        sep = "")
  } else {
    print_default(x, max = n)
  }
}


print_items.tbl <- function(x, n = Inf, n_extra = 10 * n, ...) {
  print(x, n = n, n_extra = n_extra)
}


print_modelinfo <- function(x, trained = FALSE) {
  info_list <- modelinfo(x)
  if (length(info_list)) {
    info <- info_list[[1]]
    cat("\n",
        "Model name: ", names(info_list), "\n",
        "Label: ", if (trained) "Trained ", info$label, "\n",
        sep = "")
    print_items(label_items("Package", info$packages), exdent = 2)
    print_items(label_items("Response type", info$response_types), exdent = 2)
    print_items(paste("Case weights support:", toString(info$weights)),
                exdent = 2)
    cat("Tuning grid: ", info$grid, "\n",
        "Variable importance: ", info$varimp, "\n",
        sep = "")
  }
}


print_title <- function(x, ...) {
  UseMethod("print_title")
}


print_title.default <- function(x, ...) {
  print_title(class1(x), ...)
}


print_title.character <- function(x, ...) {
  print_heading(paste0("Object of class \"", x, "\"\n"), ...)
  list(...)
}


title_level <- function(x) {
  c(x$level, 0)[1]
}
