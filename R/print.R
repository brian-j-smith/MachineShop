#' Print MachineShop Objects
#'
#' Print methods for objects defined in the \pkg{MachineShop} package.
#'
#' @name print
#' @rdname print-methods
#'
#' @param x object to print.
#' @param n integer number of models or data frame rows to show.
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
print.MLModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained, ...)
  if (level < 1) {
    newline()
    print_label("Parameters:")
    cat(str(x@params, give.attr = FALSE, list.len = n))
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


#' @rdname print-methods
#'
print.ModelFrame <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  label <- if (level < 2) style_label("Terms: ")
  print_items(formula(x), n = n, label = label)
  if (level < 1) {
    print_label("Data:")
    print_items(within(as(x, "data.frame"), remove("(names)")), n = n)
  } else if (level == 1) {
    print_fields(list("Number of observations: " = nrow(x)))
  }
  invisible(x)
}


setShowDefault("ModelFrame")


#' @rdname print-methods
#'
print.ModelRecipe <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  tbl <- summary(x, original = TRUE)
  print_fields(list(
    "Outcome{?s}: " = tbl$variable[tbl$role == "outcome"],
    "Predictor{?s}: " = tbl$variable[tbl$role == "predictor"]
  ), n = n)
  if (level < 2) {
    status <- if (is_trained(x)) "Prepared" else "Unprepared"
    print_fields(
      list(steps = if (length(x$steps)) map("char", class1, x$steps)),
      labels = c(steps = paste(status, "step{?s}: "))
    )
  }
  if (level < 1) {
    print_label("Data:")
    print_items(within(x$template, remove("(names)")), n = n)
  } else if (level == 1) {
    print_fields(list("Number of observations: " = nrow(x$template)))
  }
  invisible(x)
}


setShowDefault("ModelRecipe")


print.ModelTerms <- function(x, n = MachineShop::settings("print_max"), ...) {
  print_items(formula(x), n = n)
  invisible(x)
}


print.ModeledInput <- function(x, n = MachineShop::settings("print_max"), ...) {
  NextMethod()
  level <- nesting_level(...)
  if (level < 2) {
    newline()
    print(x@model, n = n, level = level + 1)
  }
  invisible(x)
}


print.ModeledTerms <- function(x, n = MachineShop::settings("print_max"), ...) {
  level <- nesting_level(...)
  print_items(formula(x), n = n)
  if (level < 2) {
    newline()
    print(x@model, n = n, level = level + 1)
  }
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
  dn <- dimnames(x)
  print_fields(list("Metric{?s}: " = dn[[2]]))
  if (length(dn) > 2) {
    print_fields(list("Model{?s}: " = dn[[3]]), n = n)
  }
  invisible(x)
}


#' @rdname print-methods
#'
print.PerformanceCurve <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, ...)
  xy_labels <- c(x@metrics$x@label, x@metrics$y@label)
  print_fields(list("Metrics: " = paste(c("x", "y"), "=", xy_labels)))
  newline()
  print_items(as(x, "data.frame"), n = n)
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
  print_fields(map(names, x@case_comps), labels = c(
    groups = "Grouping variable: ",
    strata = "Stratification variable: "
  ))
  newline()
  print(x@control, n = n, level = level + 1)
  invisible(x)
}


setShowDefault("Resample")


print.SelectedInput <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  NextMethod()
  level <- nesting_level(...)
  nextlevel <- level + 1
  if (level < 1) {
    hline(level)
    print_train_label("Selection set:")
    newline()
    print(x@inputs, n = n, level = nextlevel)
    hline(level)
    print(x@params@control, n = n, level = nextlevel)
  } else if (level == 1) {
    newline()
    print_train_label("Selection set:")
    print(x@inputs, n = n, level = nextlevel)
  }
  invisible(x)
}


print.SelectedModel <- function(
  x, n = MachineShop::settings("print_max"), ...
) {
  title(x, ...)
  level <- nesting_level(...)
  nextlevel <- level + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained, ...)
  if (level < 1) {
    hline(level)
    print_train_label("Selection set:")
    newline()
    print(x@models, n = n, level = nextlevel)
    hline(level)
    print(x@params@control, n = n, level = nextlevel)
    if (trained) {
      hline(level)
      print(x@steps, n = n, level = nextlevel)
    }
  } else if (level == 1) {
    newline()
    print_train_label("Selection set:")
    print(x@models, n = n, level = nextlevel)
  }
  invisible(x)
}


print.StackedModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  nextlevel <- level + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained, ...)
  if (level < 1) {
    newline()
    print_label("Parameters:")
    subset <- setdiff(names(x@params), c("base_learners", "control"))
    cat(str(x@params[subset], give.attr = FALSE, list.len = n))
    hline(level)
    print_train_label("Base learners:")
    newline()
    print(x@params$base_learners, n = n, level = nextlevel)
    hline(level)
    print(x@params$control, n = n, level = nextlevel)
    if (trained) {
      hline(level)
      print(x@steps, n = n, level = nextlevel)
    }
  }
  invisible(x)
}


print.SuperModel <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  nextlevel <- level + 1
  trained <- is_trained(x)
  print_modelinfo(x, trained = trained, ...)
  if (level < 1) {
    newline()
    print_label("Parameters:")
    subset <- setdiff(names(x@params), c("base_learners", "control", "model"))
    cat(str(x@params[subset], give.attr = FALSE, list.len = n))
    hline(level)
    print_train_label("Super learner:")
    newline()
    print(x@params$model, n = n, level = nextlevel)
    newline()
    print_train_label("Base learners:")
    newline()
    print(x@params$base_learners, n = n, level = nextlevel)
    hline(level)
    print(x@params$control, n = n, level = nextlevel)
    if (trained) {
      hline(level)
      print(x@steps, n = n, level = nextlevel)
    }
  } else if (level == 1) {
    newline()
    print_train_label("Super learner:")
    print(x@params$model, n = n, level = nextlevel)
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


#' @rdname print-methods
#'
print.TrainingStep <- function(x, n = MachineShop::settings("print_max"), ...) {
  title(x, ...)
  level <- nesting_level(...)
  selected <- which(x@grid$selected)
  if (length(x@grid$params)) {
    print_label(x@name, " grid:")
    print_items(x@grid, n = n)
  }
  if (size(x@performance, 3) > 1) {
    newline()
    print_fields(list("Selected grid row: " = selected))
    print_fields(list(value = x@grid$metrics[[1]][selected]), labels = c(
      value = paste(names(x@grid$metrics)[1], "value: ")
    ))
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
    print(x@params@control, n = n, level = nextlevel)
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
      print(x@params@control, n = n, level = nextlevel)
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
  print_fields(list("Random sample: " = x@random))
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
    monitor <- x@monitor[map("logi", isTRUE, x@monitor)]
    if (length(monitor)) {
      print_fields(list("Monitor: " = names(monitor)))
    }
    print_fields(list("Seed: " = x@seed))
  }
  invisible(x)
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


print_items <- function(x, ...) {
  UseMethod("print_items")
}


print_items.default <- function(x, ...) {
  print(x)
}


print_items.character <- function(
  x, n = Inf, label = "", indent = 0, exdent = 0, add_names = FALSE, sep = ", ",
  style = NULL, ...
) {
  lines <- ansi_strwrap(
    note_items(label, x, add_names = add_names, sep = sep),
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
  print_items(x, exdent = exdent, sep = "+", ...)
}


print_items.list <- function(x, n = Inf, ...) {
  diff <- length(x) - n
  if (diff > 0) {
    print_default(head(x, n), max = n)
    if (is.null(names(x))) names(x) <- seq_along(x)
    extra <- tail(names(x), diff)
    msg <- pluralize(
      "... with ", format_len(diff), " more element{?s}: ", "{qty(diff)}"
    )
    print_items(extra, label = msg, n = n, style = style_extra)
    newline()
  } else {
    print_default(x, max = n)
  }
}


print_items.ListOf <- function(x, n = Inf, level = 0, ...) {
  inds <- head(seq_along(x), n)
  list_names <- names(x)
  if (is.null(list_names)) list_names <- make_names_len(length(x), "Component ")
  for (i in inds) {
    hline(level, label = list_names[i])
    print(x[[i]], n = n, level = level, na.print = NULL)
    if (level < 2) newline()
  }
  n_more <- max(length(x) - n, 0)
  if (n_more) {
    extra <- tail(list_names, n_more)
    msg <- pluralize(
      "... with ", format_len(n_more), " more element{?s}: ", "{qty(n_more)}"
    )
    print_items(extra, label = msg, n = n, style = style_extra)
    if (level < 2) newline()
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


print_modelinfo <- function(x, trained = FALSE, level = 0, ...) {
  info_list <- modelinfo(x)
  if (is_empty(info_list)) return(NULL)
  info <- info_list[[1]]
  print_fields(list(
    "Model name: " =  names(info_list),
    "Label: " = paste0(if (trained) "Trained ", info$label)
  ))
  if (level < 1) {
    print_fields(info, labels = c(
      packages = "Package{?s}: ",
      response_types = "Response type{?s}: ",
      weights = "Case weights support: ",
      grid = "Tuning grid: ",
      varimp = "Variable importance: "
    ))
  }
}


print_train_label <- function(...) {
  print_label(style_bold(...))
}


style_extra <- make_ansi_style("grey")
style_label <- make_ansi_style("blue")


title <- function(x, ...) {
  heading(paste(class1(x), "object"), ...)
}
