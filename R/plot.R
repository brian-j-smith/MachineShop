#' Model Performance Plots
#'
#' Plot measures of model performance and predictor variable importance.
#'
#' @name plot
#' @rdname plot-methods
#'
#' @param x \link{calibration}, \link{confusion}, \link{lift},
#'   trained model \link{fit}, partial \link{dependence}, \link{performance},
#'   \link[=curves]{performance curve}, \link{resample}, \link{rfe}, or
#'   \link[=varimp]{variable importance} result.
#' @param diagonal logical indicating whether to include a diagonal reference
#'   line.
#' @param find numeric true positive rate at which to display reference lines
#'   identifying the corresponding rates of positive predictions.
#' @param metrics vector of numeric indexes or character names of performance
#'   metrics to plot.
#' @param n number of most important variables to include in the plot.
#' @param se logical indicating whether to include standard error bars.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metrics for trained \code{MLModel} line
#'   plots and \code{Resample} model ordering.  The original ordering is
#'   preserved if a value of \code{NULL} is given.  For \code{LiftCurve} and
#'   \code{PerformanceCurve} classes, plots are of resampled metrics aggregated
#'   by the statistic if given or of resample-specific metrics if \code{NULL}.
#' @param stats vector of numeric indexes or character names of partial
#'   dependence summary statistics to plot.
#' @param type type of plot to construct.
#' @param ... arguments passed to other methods.
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Factor response example
#'
#' fo <- Species ~ .
#' control <- CVControl()
#'
#' gbm_fit <- fit(fo, data = iris, model = GBMModel, control = control)
#' plot(varimp(gbm_fit))
#'
#' gbm_res1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbm_res2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbm_res3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' plot(gbm_res3)
#'
#' res <- c(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
#' plot(res)
#' }
#'
NULL


#' @rdname plot-methods
#'
plot.Calibration <- function(x, type = c("line", "point"), se = FALSE, ...) {
  type <- match.arg(type)

  args <- alist(.data[["Predicted"]], .data[["Mean"]])
  if (nlevels(x$Response) > 1) {
    args$color <- args$fill <- .(.data[["Response"]])
  }
  mapping <- do.call(aes, args)

  position <- "identity"

  pl <- by(x, x$Model, function(cal) {

    df <- data.frame(
      Response = cal$Response,
      Predicted = cal$Predicted,
      cal$Observed
    )
    Predicted_width <- diff(range(df$Predicted, na.rm = TRUE))

    p <- ggplot(df, mapping) +
      geom_abline(intercept = 0, slope = 1, color = "gray") +
      labs(title = cal$Model[1], x = "Predicted", y = "Observed Mean")

    if (se) if (x@smoothed) {
      p <- p +
        geom_ribbon(
          aes(ymin = .data[["Lower"]], ymax = .data[["Upper"]]),
          linetype = "blank", alpha = 0.2, na.rm = TRUE
        )
    } else {
      position <- position_dodge(width = 0.025 * Predicted_width)
      p <- p +
        geom_errorbar(
          aes(ymin = .data[["Lower"]], ymax = .data[["Upper"]]),
          width = 0.05 * Predicted_width, position = position, na.rm = TRUE
        )
    }

    switch(type,
      "line" = p + geom_line(position = position, na.rm = TRUE),
      "point" = p + geom_point(position = position, na.rm = TRUE)
    )

  }, simplify = FALSE)

  structure(as(pl, "list"), names = levels(x$Model))
}


#' @rdname plot-methods
#'
plot.ConfusionList <- function(x, ...) {
  pl <- list()
  for (name in names(x)) {
    pl[[name]] <- plot(x[[name]]) + labs(title = name)
  }
  pl
}


#' @rdname plot-methods
#'
plot.ConfusionMatrix <- function(x, ...) {
  df <- as.data.frame(proportions(as(x, "table")), responseName = "Value")
  df$Predicted <- factor(df$Predicted, rev(levels(df$Predicted)))
  ggplot(
    df, aes(.data[["Observed"]], .data[["Predicted"]], fill = .data[["Value"]])
  ) +
    geom_raster() +
    labs(fill = "Probability") +
    scale_fill_gradient(trans = "reverse") +
    coord_fixed()
}


#' @rdname plot-methods
#'
plot.LiftCurve <- function(
  x, find = numeric(), diagonal = TRUE,
  stat = MachineShop::settings("stat.Curve"), ...
) {
  x <- summary(x, stat = stat)
  p <- plot(as(x, "PerformanceCurve"), diagonal = diagonal, stat = NULL)

  if (length(find)) {
    find <- check_numeric(find, bounds = c(0, 1), include = FALSE, size = 1)
    throw(check_assignment(find))

    indices <- x["Model"]
    indices$Iteration <- x$Iteration
    tested <- by(x, indices, function(data) {
      approx(data$y, data$x, find, ties = "ordered")$y
    })

    df <- data.frame(
      x = as.numeric(tested),
      y = find,
      Model = dimnames(tested)$Model
    )
    df$Iteration <- rep(dimnames(tested)$Iteration, each = size(tested, 1))

    p <- p +
      geom_segment(
        aes(x = .data[["x"]], y = 0, xend = .data[["x"]], yend = .data[["y"]]),
        df
      ) +
      geom_segment(
        aes(x = 0, y = .data[["y"]], xend = .data[["x"]], yend = .data[["y"]]),
        df
      )
  }

  p
}


#' @rdname plot-methods
#'
plot.MLModel <- function(
  x, metrics = NULL, stat = MachineShop::settings("stat.TrainingParams"),
  type = c("boxplot", "density", "errorbar", "line", "violin"), ...
) {
  if (!is_trained(x)) throw(Error("No training results to plot."))
  map(function(step) {
    plot(step, metrics = metrics, stat = stat, type = type, ...)
  }, x@steps)
}


#' @rdname plot-methods
#'
plot.PartialDependence <- function(x, stats = NULL, ...) {
  if (any(rowSums(!is.na(x$Predictors)) > 1)) {
    throw(Error(
      "Partial dependence plots not available for interaction efffects."
    ))
  }

  if (!is.null(stats)) {
    stats <- match_indices(stats, levels(x$Statistic))
    x <- x[x$Statistic %in% stats, , drop = FALSE]
  }

  df <- x[c("Statistic", "Response", "Value")]

  args <- alist(.data[["Predictor"]], .data[["Value"]])
  if (nlevels(x$Response) > 1) args$color <- .(.data[["Response"]])
  mapping <- do.call(aes, args)

  pl <- list()
  for (varname in names(x$Predictors)) {
    df$Predictor <- x$Predictors[[varname]]
    p <- ggplot(na.omit(df), mapping)
    p <- switch_class(df$Predictor,
      "factor" = p +
        geom_crossbar(aes(ymin = after_stat(y), ymax = after_stat(y))),
      "numeric" = p + geom_line() + geom_point()
    ) + labs(x = varname) + facet_wrap(~ Statistic, scales = "free")
    pl[[varname]] <- p
  }
  pl
}


#' @rdname plot-methods
#'
plot.Performance <- function(
  x, metrics = NULL, stat = MachineShop::settings("stat.Resample"),
  type = c("boxplot", "density", "errorbar", "violin"), ...
) {
  df <- as.data.frame(x)
  if (is.null(df$Model)) df$Model <- factor("Model")

  metric_levels <- levels(df$Metric)
  if (is.null(metrics)) {
    metrics <- metric_levels
  } else {
    metrics <- match_indices(metrics, metric_levels)
    df <- df[df$Metric %in% metrics, , drop = FALSE]
  }
  df$Metric <- factor(df$Metric, metrics)

  if (is.null(stat)) stat <- function(x) 0
  stat <- check_stat(stat, convert = TRUE)
  throw(check_assignment(stat))

  df_metric <- df[df$Metric == metrics[1], , drop = FALSE]
  sorted_levels <- tapply(
    df_metric$Value, df_metric$Model, function(x) stat(na.omit(x))
  ) %>% sort %>% names
  df$Model <- factor(df$Model, sorted_levels)

  p <- ggplot(df)
  mapping <- aes(.data[["Model"]], .data[["Value"]])
  switch(match.arg(type),
    "boxplot" = p +
      geom_boxplot(mapping) +
      stat_summary(mapping, fun = mean, geom = "point") +
      labs(x = "") +
      coord_flip(),
    "density" = p +
      geom_density(aes(.data[["Value"]], color = .data[["Model"]])) +
      labs(y = "Density", color = ""),
    "errorbar" = p +
      stat_summary(mapping, fun.data = mean_se, geom = "errorbar") +
      stat_summary(mapping, fun = mean, geom = "point") +
      labs(x = "") +
      coord_flip(),
    "violin" = p +
      geom_violin(mapping) +
      stat_summary(mapping, fun = mean, geom = "point") +
      labs(x = "") +
      coord_flip()
  ) + facet_wrap(~ Metric, scales = "free")
}


#' @rdname plot-methods
#'
plot.PerformanceCurve <- function(
  x, type = c("tradeoffs", "cutoffs"), diagonal = FALSE,
  stat = MachineShop::settings("stat.Curve"), ...
) {
  x <- summary(x, stat = stat)

  args <- alist(.data[["x"]], .data[["y"]])
  if (nlevels(x$Model) > 1) args$color <- .(.data[["Model"]])
  if (!is.null(x$Iteration)) {
    args$group <- .(interaction(.data[["Model"]], .data[["Iteration"]]))
  }
  mapping <- do.call(aes, args)

  labels <- c(x = x@metrics$x@label, y = x@metrics$y@label)

  switch(match.arg(type),
    "tradeoffs" = {
      x$Cutoff <- NULL
      p <- ggplot(na.omit(x), mapping) +
        geom_path() +
        labs(x = labels["x"], y = labels["y"])

      if (diagonal) {
        p <- p + geom_abline(intercept = 0, slope = 1, color = "gray")
      }

      p
    },
    "cutoffs" = {
      df <- reshape(
        x, varying = c("x", "y"), v.names = "y", times = labels,
        timevar = "Metric", direction = "long"
      )
      names(df)[names(df) == "Cutoff"] <- "x"

      ggplot(na.omit(df), mapping) +
        geom_line() +
        labs(x = "Cutoff", y = "Performance") +
        facet_wrap(~ Metric)
    }
  )
}


#' @rdname plot-methods
#'
plot.Resample <- function(
  x, metrics = NULL, stat = MachineShop::settings("stat.Resample"),
  type = c("boxplot", "density", "errorbar", "violin"), ...
) {
  plot(performance(x), metrics = metrics, stat = stat, type = type)
}


#' @rdname plot-methods
#'
plot.TrainingStep <- function(
  x, metrics = NULL, stat = MachineShop::settings("stat.TrainingParams"),
  type = c("boxplot", "density", "errorbar", "line", "violin"), ...
) {
  type <- match.arg(type)
  if (type == "line") {
    stat <- check_stat(stat, convert = TRUE)
    throw(check_assignment(stat))

    params <- unnest_params(summary(x)$params)
    stats <- performance(x) %>%
      apply(c(3, 2), function(x) stat(na.omit(x))) %>%
      TabularArray %>%
      as.data.frame
    df <- data.frame(
      x = params[[1]],
      Value = stats$Value,
      Metric = stats$Metric
    )

    metric_levels <- levels(df$Metric)
    if (is.null(metrics)) {
      metrics <- metric_levels
    } else {
      metrics <- match_indices(metrics, metric_levels)
      df <- df[df$Metric %in% metrics, , drop = FALSE]
    }
    df$Metric <- factor(df$Metric, metrics)

    indices <- map("logi", function(x) length(unique(x)), params[-1])
    args <- alist(.data[["x"]], .data[["Value"]])
    if (any(indices)) {
      df$Group <- interaction(params[-1][indices], sep = ":")
      args$color <- args$shape <- .(.data[["Group"]])
    } else {
      args$group <- 1
    }
    mapping <- do.call(aes, args)

    ggplot(df, mapping) +
      geom_line(stat = "summary", fun = mean) +
      geom_point(stat = "summary", fun = mean) +
      labs(x = names(params)[1]) +
      facet_wrap(~ Metric, scales = "free")
  } else {
    plot(performance(x), metrics = metrics, stat = stat, type = type, ...)
  }
}


#' @rdname plot-methods
#'
plot.VariableImportance <- function(x, n = Inf, ...) {
  x <- update(x)
  scales <- if (any(is.na(x@scale))) "free" else "fixed"
  x <- head(x, n)
  var_names <- rownames(x)
  df <- cbind(stack(x), variables = factor(var_names, rev(var_names)))
  ggplot(df, aes(.data[["variables"]], .data[["values"]])) +
    geom_bar(stat = "identity") +
    labs(x = "Variable", y = "Importance") +
    coord_flip() +
    facet_wrap(~ ind, scales = scales)
}
