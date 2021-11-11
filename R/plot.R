#' Model Performance Plots
#'
#' Plot measures of model performance and predictor variable importance.
#'
#' @name plot
#' @rdname plot-methods
#'
#' @param x \link{calibration}, \link{confusion}, \link{lift},
#'   trained model \link{fit}, partial \link{dependence}, \link{performance},
#'   \link[=curves]{performance curve}, \link{resample}, or
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
#'   plots and \code{Resamples} model ordering.  For \code{LiftCurve} and
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

  args <- list(~ Predicted, ~ Mean)
  if (nlevels(x$Response) > 1) args$color <- args$fill <- ~ Response
  mapping <- do.call(aes_, args)

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
      p <- p + geom_ribbon(aes_(ymin = ~ Lower, ymax = ~ Upper),
                           linetype = "blank", alpha = 0.2, na.rm = TRUE)
    } else {
      position <- position_dodge(width = 0.025 * Predicted_width)
      p <- p + geom_errorbar(aes_(ymin = ~ Lower, ymax = ~ Upper),
                             width = 0.05 * Predicted_width,
                             position = position, na.rm = TRUE)
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
  df <- as.data.frame(prop.table(as(x, "table")), responseName = "Value")
  df$Predicted <- factor(df$Predicted, rev(levels(df$Predicted)))
  ggplot(df, aes_(~ Observed, ~ Predicted, fill = ~ Value)) +
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
    if (find < 0 || find > 1) {
      throw(Warning("'find' rate outside of 0 to 1 range"))
    }

    indices <- x["Model"]
    indices$Resample <- x$Resample
    tested <- by(x, indices, function(data) {
      approx(data$y, data$x, find, ties = "ordered")$y
    })

    df <- data.frame(
      x = as.numeric(tested),
      y = find,
      Model = dimnames(tested)$Model
    )
    df$Resample <- rep(dimnames(tested)$Resample, each = size(tested, 1))

    p <- p +
      geom_segment(aes_(x = ~ x, y = 0, xend = ~ x, yend = ~ y), df) +
      geom_segment(aes_(x = 0, y = ~ y, xend = ~ x, yend = ~ y), df)
  }

  p
}


#' @rdname plot-methods
#'
plot.MLModel <- function(
  x, metrics = NULL, stat = MachineShop::settings("stat.Trained"),
  type = c("boxplot", "density", "errorbar", "line", "violin"), ...
) {
  if (!is_trained(x)) throw(Error("no training results to plot"))

  stat <- check_stat(stat, convert = TRUE)
  throw(check_assignment(stat))
  type <- match.arg(type)

  map(function(train_step) {
    perf <- train_step@performance
    if (type == "line") {
      grid <- unnest(train_step@grid$params)
      stats <- apply(perf, c(3, 2), function(x) stat(na.omit(x))) %>%
        TabularArray %>%
        as.data.frame
      df <- data.frame(
        x = grid[[1]],
        Value = stats$Value,
        Metric = stats$Metric
      )

      metriclevels <- levels(df$Metric)
      if (is.null(metrics)) {
        metrics <- metriclevels
      } else {
        metrics <- match_indices(metrics, metriclevels)
        df <- df[df$Metric %in% metrics, , drop = FALSE]
      }
      df$Metric <- factor(df$Metric, metrics)

      indices <- map_logi(function(x) length(unique(x)), grid[-1])
      args <- list(~ x, ~ Value)
      if (any(indices)) {
        df$Group <- interaction(grid[-1][indices])
        args$color <- args$shape <- ~ Group
      } else {
        args$group <- 1
      }
      mapping <- do.call(aes_, args)

      ggplot(df, mapping) +
        geom_line(stat = "summary", fun = mean) +
        geom_point(stat = "summary", fun = mean) +
        labs(x = names(grid)[1]) +
        facet_wrap(~ Metric, scales = "free")
    } else {
      plot(perf, metrics = metrics, stat = stat, type = type, ...)
    }
  }, x@train_steps)
}


#' @rdname plot-methods
#'
plot.PartialDependence <- function(x, stats = NULL, ...) {
  if (any(rowSums(!is.na(x$Predictors)) > 1)) {
    msg <- "partial dependence plots not available for interaction efffects"
    throw(Error(msg))
  }

  if (!is.null(stats)) {
    stats <- match_indices(stats, levels(x$Statistic))
    x <- x[x$Statistic %in% stats, , drop = FALSE]
  }

  df <- x[c("Statistic", "Response", "Value")]

  args <- list(~ Predictor, ~ Value)
  if (nlevels(x$Response) > 1) args$color <- ~ Response
  mapping <- do.call(aes_, args)

  pl <- list()
  for (varname in names(x$Predictors)) {
    df$Predictor <- x$Predictors[[varname]]
    p <- ggplot(na.omit(df), mapping)
    p <- switch_class(df$Predictor,
      "factor" = p + geom_crossbar(aes_(ymin = ~ ..y.., ymax = ~ ..y..)),
      "numeric" = p + geom_line() + geom_point()
    ) + labs(x = varname) + facet_wrap(~ Statistic, scales = "free")
    pl[[varname]] <- p
  }
  pl
}


#' @rdname plot-methods
#'
plot.Performance <- function(
  x, metrics = NULL, stat = MachineShop::settings("stat.Resamples"),
  type = c("boxplot", "density", "errorbar", "violin"), ...
) {
  df <- as.data.frame(x)
  if (is.null(df$Model)) df$Model <- factor("Model")

  metriclevels <- levels(df$Metric)
  if (is.null(metrics)) {
    metrics <- metriclevels
  } else {
    metrics <- match_indices(metrics, metriclevels)
    df <- df[df$Metric %in% metrics, , drop = FALSE]
  }
  df$Metric <- factor(df$Metric, metrics)

  stat <- check_stat(stat, convert = TRUE)
  throw(check_assignment(stat))

  firstmetric <- df[df$Metric == metrics[1], , drop = FALSE]
  sortedlevels <- tapply(firstmetric$Value, firstmetric$Model,
                         function(x) stat(na.omit(x))) %>% sort %>% names
  df$Model <- factor(df$Model, sortedlevels)

  p <- ggplot(df)
  switch(match.arg(type),
    "boxplot" = p + geom_boxplot(aes_(~ Model, ~ Value)) +
      stat_summary(aes_(~ Model, ~ Value), fun = mean, geom = "point") +
      labs(x = "") +
      coord_flip(),
    "density" = p + geom_density(aes_(~ Value, color = ~ Model)) +
      labs(y = "Density", color = ""),
    "errorbar" = p + stat_summary(aes_(~ Model, ~ Value),
                                  fun.data = mean_se,
                                  geom = "errorbar") +
      stat_summary(aes_(~ Model, ~ Value), fun = mean, geom = "point") +
      labs(x = "") +
      coord_flip(),
    "violin" = p + geom_violin(aes_(~ Model, ~ Value)) +
      stat_summary(aes_(~ Model, ~ Value), fun = mean, geom = "point") +
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

  args <- list(~ x, ~ y)
  if (nlevels(x$Model) > 1) args$color <- ~ Model
  if (!is.null(x$Resample)) args$group <- ~ interaction(Model, Resample)
  mapping <- do.call(aes_, args)

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
      df <- reshape(x, varying = c("x", "y"), v.names = "y", times = labels,
                    timevar = "Metric", direction = "long")
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
plot.Resamples <- function(
  x, metrics = NULL, stat = MachineShop::settings("stat.Resamples"),
  type = c("boxplot", "density", "errorbar", "violin"), ...
) {
  plot(performance(x), metrics = metrics, stat = stat, type = type)
}


#' @rdname plot-methods
#'
plot.VariableImportance <- function(x, n = Inf, ...) {
  x <- head(x, n)
  var_names <- rownames(x)
  df <- cbind(stack(x), variables = factor(var_names, rev(var_names)))
  p <- ggplot(df, aes_(~ variables, ~ values)) +
    geom_bar(stat = "identity") +
    labs(x = "Variable", y = "Importance") +
    coord_flip()
  if (nlevels(df$ind) > 1) p <- p + facet_wrap(~ ind)
  p
}
