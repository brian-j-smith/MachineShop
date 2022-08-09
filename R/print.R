#' Print MachineShop Objects
#'
#' Print methods for objects defined in the \pkg{MachineShop} package.
#'
#' @name print
#' @rdname print-methods
#'
#' @param x object to print.
#' @param n integer number of models or data frame rows to show.
#' @param id logical indicating whether to show object identifiers.
#' @param data logical indicating whether to show model data.
#' @param ... arguments passed to other methods, including the one described
#'   below.
#'   \describe{
#'     \item{\code{level} = 0}{current nesting level of the corresponding
#'     object in recursive calls to \code{print}.  The amount of information
#'     displayed decreases and increases with positive and negative levels,
#'     respectively.}
#'   }
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
  title(x, pad = FALSE, ...)
  print_items(as(x, "matrix"), n = n)
  invisible(x)
}


#' @rdname print-methods
#'
print.Calibration <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setShowDefault("Calibration")


print.ConfusionList <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, ...)
  level <- nesting_level(...)
  NextMethod(level = level + 1)
}


print.ConfusionMatrix <- function(x, ...) {
  title(x, pad = FALSE, ...)
  print(as(x, "table"))
  invisible(x)
}


setShowDefault("ConfusionMatrix")


print.ConfusionSummary <- function(x, ...) {
  acc <- x@accuracy
  acc_se <- sqrt(acc * (1 - acc) / x@total)
  print_fields(list(
    "Number of responses: " = x@total,
    "Accuracy (SE): " = paste0(format(acc), " (", format(acc_se), ")"),
    "Majority class: " = x@majority,
    "Kappa: " = x@kappa2
  ))
  newline()
  print(as(x, "matrix"))
  invisible(x)
}


setShowDefault("ConfusionSummary")


#' @rdname print-methods
#'
print.DiscreteVariate <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, pad = FALSE, ...)
  print_items(as(x, "numeric"), n = n)
  print_fields(list("Range: " = c(x@min, x@max)))
  invisible(x)
}


setShowDefault("DiscreteVariate")


print.EnsembleModel <- function(
  x, n = MachineShop::settings("print_max"), id = FALSE, ...
) {
  title(x, ...)
  level <- nesting_level(...)
  nextlevel <- level + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained, ...)
  if (level < 1) {
    hline(level)
    print_train_label("Models:")
    newline()
    print(x@candidates, n = n, level = nextlevel, id = id)
    hline(level)
    print(x@params, n = n, level = nextlevel)
    if (trained) {
      hline(level)
      print(x@steps, n = n, level = nextlevel)
    }
  } else if (level == 1) {
    newline()
    print_train_label("Models:")
    print(x@candidates, n = n, level = nextlevel, id = id)
  }
  invisible(x)
}


#' @rdname print-methods
#'
print.ListOf <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_items(x, n = n, ...)
  invisible(x)
}


setShowDefault("ListOf")


setShowDefault("MLControl")


#' @rdname print-methods
#'
print.MLControl <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_control(x, n = n, ...)
}


print.BootControl <- function(x, ...) {
  fields <- list("Samples: " = x@samples)
  NextMethod(fields = fields)
}


print.CVControl <- function(x, ...) {
  fields <- list("Folds: " = x@folds, "Repeats: " = x@repeats)
  NextMethod(fields = fields)
}


print.NullControl <- function(x, ...) {
  title(x, ...)
  print_fields(list("Label: " = x@label))
  invisible(x)
}


print.OOBControl <- function(x, ...) {
  fields <- list("Samples: " = x@samples)
  NextMethod(fields = fields)
}


print.SplitControl <- function(x, ...) {
  fields = list("Training proportion: " = x@prop)
  NextMethod(fields = fields)
}


#' @rdname print-methods
#'
print.MLMetric <- function(x, ...) {
  title(x, ...)
  level <- nesting_level(...)
  print_fields(list(
    "Metric name: " = x@name,
    "Label: " = x@label,
    "Maximize: " = x@maximize
  ))
  if (level < 1) {
    info <- metricinfo(x)[[1]]
    newline()
    print_label("Arguments:")
    print(info$arguments)
    newline()
    print_label("Types:")
    print(info$response_types)
  }
  invisible(x)
}


setShowDefault("MLMetric")


#' @rdname print-methods
#'
print.MLModel <- function(
  x, n = MachineShop::settings("print_max"), id = FALSE, ...
) {
  title(x, ...)
  level <- nesting_level(...)
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained, id = id, ...)
  if (level < 1) {
    newline()
    print_str(x@params, label = "Parameter{?s}:", n = n)
    if (trained) {
      newline()
      print(x@steps, n = n, level = level + 1)
    }
  }
  invisible(x)
}


setShowDefault("MLModel")


print.MLModelFit <- function(x, ...) {
  print(unMLModelFit(x), ...)
  invisible(x)
}


setShowDefault("MLModelFit")


#' @rdname print-methods
#'
print.MLModelFunction <- function(x, ...) {
  title(x, ...)
  level <- nesting_level(...)
  print_modelinfo(x, ...)
  if (level < 1) {
    newline()
    print_label("Arguments:")
    print(args(x))
  }
  invisible(x)
}


setShowDefault("MLModelFunction")


print.MLOptimization <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, ...)
  level <- nesting_level(...)
  print_fields(list("Label: " = x@label))
  if (level < 1) {
    print_fields(list(
      "Package{?s}: " = unname(x@packages),
      "Monitor: " = x@monitor
    ), add_names = TRUE)
    if (length(x@params)) print_str(x@params, label = "Parameter{?s}:", n = n)
  }
  invisible(x)
}


setShowDefault("MLOptimization")


#' @rdname print-methods
#'
print.ModelFrame <- function(
  x, n = MachineShop::settings("print_max"), id = FALSE, data = TRUE, ...
) {
  title(x, ...)
  level <- nesting_level(...)
  if (id) print_id(x)
  label <- if (level < 2) style_label("Terms: ")
  print_items(formula(x), n = n, label = label)
  if (data) print_data(as(x, "data.frame"), n = n, level = level)
  invisible(x)
}


setShowDefault("ModelFrame")


#' @rdname print-methods
#'
print.ModelRecipe <- function(
  x, n = MachineShop::settings("print_max"), id = FALSE, data = TRUE, ...
) {
  title(x, ...)
  level <- nesting_level(...)
  if (id) print_id(x)
  tbl <- summary(x, original = TRUE)
  print_fields(list(
    "Outcome{?s}: " = tbl$variable[tbl$role == "outcome"],
    "Predictor{?s}: " = tbl$variable[tbl$role == "predictor"]
  ), n = n)
  if (level < 2) {
    steps <- map("char", class1, x$steps)
    step_label <- paste(if (is_trained(x)) "Trained" else "Untrained", "step")
    if (id) {
      steps <- structure(map("char", getElement, x$steps, "id"), names = steps)
      step_label <- paste(step_label, "ID")
    }
    print_fields(
      list(steps = steps), labels = c(steps = paste0(step_label, "{?s}: ")),
      add_names = TRUE, n = n
    )
  }
  if (level < 1) print_fields(list("Tuning grid: " = has_grid(x)))
  if (data) print_data(as_tibble(as.data.frame(x)), n = n, level = level)
  invisible(x)
}


setShowDefault("ModelRecipe")


#' @rdname print-methods
#'
print.ModelSpecification <- function(
  x, n = MachineShop::settings("print_max"), id = FALSE, ...
) {
  title(x, ...)
  level <- nesting_level(...)
  nextlevel <- level + 1
  if (id) {
    print_id(x)
    newline()
  }
  show_all <- length(x@grid) && level < 1
  print(x@input, n = n, level = nextlevel, id = show_all)
  if (!is(x@model, "NullModel")) {
    newline()
    print(x@model, n = n, level = nextlevel, id = show_all)
  }
  if (show_all) {
    newline()
    heading("Grid", level = nextlevel)
    print_items(x@grid, n = n)
    newline()
    print(x@params, n = n, level = nextlevel)
  }
}


setShowDefault("ModelSpecification")


print.ModelTerms <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_items(formula(x), n = n)
  invisible(x)
}


print.ParameterGrid <- function(x, ...) {
  NextMethod()
  newline()
  print(as(x, "parameters"))
  invisible(x)
}


setShowDefault("ParameterGrid")


print.ParsnipModel <- function(x, ...) {
  x@params <- x@params$object$args
  NextMethod()
}


#' @rdname print-methods
#'
print.Performance <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  dn <- dimnames(x)
  print_fields(list("Metric{?s}: " = dn[[2]]))
  if (length(dn) > 2) {
    print_fields(list("Model{?s}: " = dn[[3]]), n = n)
  }
  newline()
  print(x@control, level = level + 1)

  invisible(x)
}


#' @rdname print-methods
#'
print.PerformanceCurve <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, ...)
  level <- nesting_level(...)
  xy_labels <- c(x@metrics$x@label, x@metrics$y@label)
  print_fields(list("Metrics: " = paste(c("x", "y"), "=", xy_labels)))
  newline()
  print_items(as(x, "data.frame"), n = n)
  if (!is(x@control, "NullControl")) {
    newline()
    print(x@control, level = level + 1)
  }
  invisible(x)
}


setShowDefault("PerformanceCurve")


print.PerformanceDiffTest <- function(x, ...) {
  title(x, ...)
  print_fields(list(
    "Upper diagonal: " = "mean differences (Model1 - Model2)",
    "Lower diagonal: " = "p-values",
    "P-value adjustment method: " = x@adjust
  ))
  newline()
  print(as(x, "array"))
  invisible(x)
}


print.RandomGridSearch <- function(x, ...) {
  NextMethod()
  print_fields(list("Samples: " = x@size))
  invisible(x)
}


#' @rdname print-methods
#'
print.RecipeGrid <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  print_items(as(x, "tbl_df"), n = n)
  invisible(x)
}


setShowDefault("RecipeGrid")


#' @rdname print-methods
#'
print.Resample <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  print_fields(list("Model{?s}: " = levels(x$Model)), n = n)
  if (level >= 1) {
    print_fields(list(
      "Grouping variable: " = names(x@vars[["Grouping"]]),
      "Stratification variable: " = names(x@vars[["Stratification"]])
    ))
  } else if (length(x@vars) > 1) {
    print_label("Sampling variables:")
    print_items(x@vars, n = n)
  }
  newline()
  print(x@control, n = n, level = level + 1)
  invisible(x)
}


setShowDefault("Resample")


print.SelectedInput <- function(
  x, n = MachineShop::settings("print_max"), id = FALSE, ...
) {
  NextMethod()
  level <- nesting_level(...)
  nextlevel <- level + 1
  if (level < 1) {
    hline(level)
    print_train_label("Selection set:")
    newline()
    print(x@candidates, n = n, level = nextlevel, id = id, data = FALSE)
    hline(level)
    print(x@params, n = n, level = nextlevel)
  } else if (level == 1) {
    newline()
    print_train_label("Selection set:")
    print(x@candidates, n = -n, level = nextlevel, id = id, data = FALSE)
  }
  invisible(x)
}


print.SequentialOptimization <- function(x, ...) {
  NextMethod()
  level <- nesting_level(...)
  if (level < 1) {
    print_fields(list("Fallback grid samples: " = if (x@random) x@random))
  }
  invisible(x)
}


format.SurvMatrix <- function(x, ...) {
  format(as(x, "matrix"), ...)
}


#' @rdname print-methods
#'
print.SurvMatrix <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, pad = FALSE, ...)
  print_items(as(x, "matrix"), n = n)
  print_fields(list(
    "Time{?s}: " = x@times,
    "Distribution: " = x@distr
  ), n = n)
  invisible(x)
}


setShowDefault("SurvMatrix")


#' @rdname print-methods
#'
print.SurvTimes <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, pad = FALSE, ...)
  print_items(as(x, "numeric"), n = n)
  print_fields(list("Distribution: " = x@distr))
  invisible(x)
}


setShowDefault("SurvTimes")


print.TabularArray <- function(x, ...) {
  print(as(x, "array"))
  invisible(x)
}


setShowDefault("TabularArray")


print.TrainingParams <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, ...)
  level <- nesting_level(...)
  nextlevel <- level + 1
  if (length(x@options)) {
    print_str(x@options, label = "Option{?s}:", n = n)
    newline()
  }
  if (is_optim_method(x)) {
    print(x@optim, n = n, level = nextlevel)
    newline()
    print(x@control, n = n, level = nextlevel)
    if (nextlevel < 0) {
      newline()
      print_label("Metrics:")
      print(x@metrics)
      newline()
      print_fields(list("Cutoff: " = x@cutoff))
      newline()
      print_label("Stat:")
      print(x@stat)
    }
  } else {
    print(x@control, n = n, level = nextlevel)
  }
  invisible(x)
}


setShowDefault("TrainingParams")


#' @rdname print-methods
#'
print.TrainingStep <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  selected <- which(x@log$selected)
  print_fields(list("Optimization method: " = x@method))
  print_label(x@name, " log:")
  print_items(x@log, n = n)
  if (ndim(x@performance) > 2) {
    newline()
    print_fields(list(
      "Selected row: " = selected,
      "Metric: " = unlist(x@log$metrics[selected, 1])
    ), add_name = TRUE)
  }
  invisible(x)
}


setShowDefault("TrainingStep")


print.TunedInput <- function(x, n = MachineShop::settings("print_max"), ...) {
  NextMethod()
  level <- nesting_level(...)
  nextlevel <- level + 1
  if (level < 1) {
    newline()
    grid <- x@grid
    if (isS4(grid)) {
      print(grid, level = nextlevel)
    } else {
      heading("Grid", level = nextlevel)
      print_items(grid, n = n)
    }
    newline()
    print(x@params, n = n, level = nextlevel)
  }
  invisible(x)
}


print.TunedModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  nextlevel <- level + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained, ...)
  if (level < 2) {
    newline()
    print(x@model, n = n, level = nextlevel)
    if (level < 1) {
      newline()
      grid <- x@grid
      if (isS4(grid)) {
        print(grid, level = nextlevel)
      } else {
        heading("Grid", level = nextlevel)
        print_items(grid, n = n)
      }
      newline()
      print(x@params, n = n, level = nextlevel)
      if (trained) {
        newline()
        print(x@steps, n = n, level = nextlevel)
      }
    }
  }
  invisible(x)
}


print.TuningGrid <- function(x, ...) {
  title(x, ...)
  print_fields(list("Grid size{?s}: " = x@size), add_names = TRUE)
  print_fields(list("Random samples: " = x@random))
  invisible(x)
}


setShowDefault("TuningGrid")


#' @rdname print-methods
#'
print.VariableImportance <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, pad = FALSE, ...)
  print_items(as(x, "data.frame"), n = n)
  invisible(x)
}


setShowDefault("VariableImportance")


#################### Print Utility Functions ####################


format_len <- function(x) format(x, big.mark = ",")


heading <- function(x, level = 0, pad = TRUE, ...) {
  style <- if (level < 0) {
    make_ansi_style("magenta")
  } else if (level == 0) {
    make_ansi_style("green")
  }
  hline(level, label = style_bold(x), trim = level, style = style)
  if (level < 2 && pad) newline()
}


hline <- function(symbol = 0, label = character(), trim = 0, style = NULL) {
  if (is.numeric(symbol)) {
    symbols <- c("-", "=", ".", "_", "~")
    symbol <- symbols[symbol %% length(symbols) + 1]
  }
  freespace <- max(console_width(), 30)
  if (length(label)) {
    nsymbol <- 3
    str <- strrep(symbol, nsymbol)
    freespace <- freespace - nsymbol - 2
    str[2] <- style_reset(ansi_strtrim(label, freespace - nsymbol))
    if (trim < 1) {
      freespace <- freespace - ansi_nchar(str[2])
      str[3] <- strrep(symbol, freespace)
    } else if (trim == 1) {
      str[3] <- str[1]
    }
    str <- paste(str, collapse = " ")
  } else {
    str <- strrep(symbol, freespace)
  }
  if (!is.null(style)) str <- style(str)
  cat(str, "\n", sep = "")
}


nesting_level <- function(level = 0, ...) {
  level
}


new_print_progress <- function() {
  res <- function(...) NULL
  body(res) <- bquote(print_progress(..., start_time = .(Sys.time())))
  res
}


newline <- function() {
  cat("\n")
}


print_control <- function(x, fields = NULL, n = Inf, ...) {
  level <- nesting_level(...)
  title(x, ...)
  print_fields(c(
    list("Label: " = x@label),
    fields
  ))
  if (level < 1) {
    print_fields(list("Case-weighted metrics: " = x@weights))
    print_fields(x@predict, labels = c(
      times = "Survival time{?s}: ",
      distr = "Distribution: ",
      method = "Method: "
    ), "Prediction parameters", n = n)
    print_fields(x@strata, labels = c(
      breaks = "Breaks: ",
      nunique = "Unique numeric threshold: ",
      prop = "Minimum proportion: ",
      size = "Minimum size: "
    ), "Stratification parameters")
    print_fields(list(
      "Monitor: " = x@monitor,
      "Seed: " = unname(x@seed)
    ), add_names = TRUE)
  }
  invisible(x)
}


print_data <- function(x, n = Inf, level = 0) {
  if (level < 1) {
    print_label("Data:")
    x[["(names)"]] <- NULL
    print_items(x, n = n)
  } else if (level == 1) {
    print_fields(list("Number of observations: " = nrow(x)))
  }
}


print_default <- function(x, max = NULL, ...) {
  if (identical(max, Inf)) max <- .Machine$integer.max
  print(x, max = max, ...)
}


print_fields <- function(x, labels = character(), heading = character(), ...) {
  if (is_empty(labels)) {
    labels <- names(x)
    names(labels) <- labels
  }
  label_names <- names(labels)
  nzlengths <- lengths(x[label_names]) > 0
  if (any(nzlengths)) {
    indent <- 2 * (length(heading) > 0)
    if (indent) print_label(heading)
    for (name in label_names[nzlengths]) {
      print_items(
        as_string(x[[name]], sep = NULL), label = style_label(labels[name]),
        indent = indent, exdent = indent + 2, ...
      )
    }
  }
}


print_id <- function(x) {
  print_fields(list("ID: " = x@id))
}


print_items <- function(x, ...) {
  UseMethod("print_items")
}


print_items.default <- function(x, ...) {
  print(x)
}


print_items.character <- function(
  x, n = Inf, label = "", indent = 0, exdent = 0, add_names = FALSE,
  add_size = FALSE, sep = ", ", style = NULL, ...
) {
  lines <- ansi_strwrap(
    note_items(label, x, add_names = add_names, add_size = add_size, sep = sep),
    indent = indent, exdent = exdent
  )
  max_lines <- ceiling(log(n))
  if (length(lines) > max_lines && length(x) > 1) {
    nlines <- max(max_lines, 1)
    lines <- head(lines, nlines)
    key <- trimws(sep)
    pattern <- paste0("[", key, "](?=([^`]*`[^`]*`)*[^`]*$)[^\\S]")
    last_pattern <- paste0(pattern, "(?:.(?!", pattern, "))+$")
    last_line <- gsub(last_pattern, key, lines[nlines], perl = TRUE)
    last_line <- ansi_strtrim(last_line, console_width() - 4)
    lines[nlines] <- paste0(last_line, style_extra(" ..."))
  }
  if (length(style)) lines <- style(lines)
  writeLines(lines)
}


print_items.data.frame <- function(x, ...) {
  print_items.matrix(format(x), ...)
}


print_items.formula <- function(x, exdent = 2, ...) {
  x <- deparse1(x)
  split <- regexpr("[+]", x)
  if (split > 0) x <- c(substring(x, 1, split - 1), substring(x, split + 1))
  print_items(x, exdent = exdent, sep = " + ", ...)
}


print_items.ListOf <- function(x, n = Inf, level = 0, ...) {
  if (is_empty(x)) {
    hline(level, label = "ListOf()")
  } else {
    labels <- map("char", function(name, i) {
      if (nzchar(name)) {
        prefix <- "$"
        label <- deparse(as.name(name), backtick = TRUE)
      } else {
        prefix <- NULL
        label <- paste0("[[", i, "]]")
      }
      if (i <= n) {
        hline(level, label = paste0(prefix, label))
        print(x[[i]], n = n, level = level, na.print = NULL, ...)
        if (level < 2) newline()
      }
      label
    }, make_names_along(x, "", unique = FALSE), seq_along(x))
    n_more <- max(length(x) - n, 0)
    if (n_more) {
      extra <- tail(labels, n_more)
      msg <- pluralize(
        "... with ", format_len(n_more), " more element{?s}: ", "{qty(n_more)}"
      )
      print_items(extra, label = msg, n = n, style = style_extra)
      if (level < 2) newline()
    }
  }
}


print_items.matrix <- function(x, n = Inf, ...) {
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
  msg <- NULL
  if (diff_rows) {
    msg <- pluralize(format_len(diff_rows), " more row{?s}", "{qty(diff_rows)}")
  }
  if (diff_cols) {
    msg <- c(
      msg,
      pluralize(format_len(diff_cols), " more column{?s}: ", "{qty(diff_cols)}")
    )
    print_extra <- function(msg) {
      extra <- tail(colnames(x), diff_cols)
      print_items(extra, label = msg, n = n, style = style_extra)
    }
  } else {
    print_extra <- function(msg) cat(style_extra(msg), "\n", sep = "")
  }
  if (length(msg)) {
    print_extra(paste("... with", as_string(msg, conj = "and")))
  }
}


print_items.numeric <- function(x, n = Inf, ...) {
  diff <- length(x) - n
  if (diff > 0) {
    print_default(head(x, n), max = n)
    msg <- pluralize(
      "... with ", format_len(diff), " more value{?s}", "{qty(diff)}"
    )
    cat(style_extra(msg), "\n", sep = "")
  } else {
    print_default(x, max = n)
  }
}


print_items.tbl <- function(x, n = Inf, ...) {
  print(x, n = n, max_footer_lines = ceiling(log(n)))
}


print_label <- function(...) {
  cat(style_label(...), "\n", sep = "")
}


print_modelinfo <- function(x, trained = FALSE, level = 0, id = FALSE, ...) {
  info_list <- modelinfo(x)
  if (is_empty(info_list)) return(NULL)
  info <- info_list[[1]]
  print_fields(list(
    "Model name: " =  names(info_list),
    "Label: " = paste0(if (trained) "Trained ", info$label)
  ))
  if (id) print_id(x)
  if (level < 1) {
    print_fields(info, labels = c(
      packages = "Package{?s}: ",
      response_types = "Response type{?s}: ",
      weights = "Case weights support: ",
      na.rm = "Missing case removal: ",
      grid = "Tuning grid: ",
      varimp = "Variable importance: "
    ))
  }
}


print_progress <- function(
  scores, max_iter = Inf, method = character(), items = list(),
  n = MachineShop::settings("print_max"), metric = numeric(),
  start_time = Sys.time(), pad = TRUE
) {
  iter <- length(scores)
  hline(label = paste0(
    "Iteration ", iter, if (is.finite(max_iter)) paste0(" of ", max_iter)
  ))
  newline()
  selected <- which.max(scores)
  if (selected == iter) selected <- paste0(">", selected, "<")
  print_fields(list("Optimization method: " = method))
  print_fields(items, add_size = TRUE, add_names = TRUE, n = n)
  print_fields(list(
    "Metric: " = metric,
    "Current maximum score (iteration): " = as_string(list(
      max(scores), paste0("(", selected, ")")
    ), sep = " "),
    "Elapsed time: " = round(difftime(Sys.time(), start_time), 1)
  ), add_names = TRUE)
  if (pad && iter < max_iter) newline()
}


print_str <- function(x, label, n = Inf) {
  print_label(pluralize(label, "{qty(length(x))}"))
  cat(str(x, give.attr = FALSE, list.len = n))
}


print_train_label <- function(...) {
  print_label(style_bold(...))
}


style_extra <- make_ansi_style("grey")
style_label <- make_ansi_style("blue")


title <- function(x, ...) {
  heading(paste(class1(x), "object"), ...)
}
