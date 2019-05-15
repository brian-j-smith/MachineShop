#' Model Performance Plots
#' 
#' Plot measures of model performance and predictor variable importance.
#'  
#' @name plot
#' @rdname plot-methods
#' 
#' @param x object to plot.
#' @param diagonal logical indicating whether to include a diagonal reference
#' line.
#' @param metrics vector of numeric indexes or character names of performance
#' metrics to plot.
#' @param stat function to compute a summary statistic on resampled values for
#' \code{MLModelTune} line plots and \code{Resamples} model sorting.  For
#' \code{Curves} and \code{Lift} classes, plots are of resampled metrics
#' aggregated by the statistic if given or of resample-specific metrics if
#' \code{NULL}.
#' @param type type of plot to construct.
#' @param ... arguments passed to other methods.
#' 
#' @seealso \code{\link{performance}},  \code{\link{resample}},
#' \code{\link{diff}}, \code{\link{tune}}, \code{\link{calibration}},
#' \code{\link{confusion}}, \code{\link{lift}}, \code{\link{dependence}},
#' \code{\link{varimp}}
#' 
plot.Performance <- function(x, metrics = NULL, stat = base::mean,
                             type = c("boxplot", "density", "errorbar",
                                      "violin"), ...) {
  df <- as.data.frame.table(x)
  if (length(dim(x)) <= 2) df$Var3 <- factor("Model")
  orderednames <- match(c("Var1", "Var2", "Var3", "Freq"), names(df))
  names(df)[orderednames] <- c("resample", "metric", "model", "y")
  
  metriclevels <- levels(df$metric)
  if (is.null(metrics)) {
    metrics <- metriclevels
  } else {
    metrics <- match_indices(metrics, metriclevels)
    df <- df[df$metric %in% metrics, , drop = FALSE]
  }
  df$metric <- factor(df$metric, metrics)
  
  firstmetric <- df[df$metric == metrics[1], , drop = FALSE]
  sortedlevels <- tapply(firstmetric$y, firstmetric$model,
                         function(x) stat(na.omit(x))) %>% sort %>% names
  df$model <- factor(df$model, sortedlevels)
  
  p <- ggplot(df)
  switch(match.arg(type),
         "boxplot" = p + geom_boxplot(aes(model, y)) +
           stat_summary(aes(model, y), fun.y = mean, geom = "point") +
           labs(x = "", y = "Values") +
           coord_flip(),
         "density" = p + geom_density(aes(y, color = model)) +
           labs(x = "Values", y = "Density", color = ""),
         "errorbar" = p + stat_summary(aes(model, y),
                                       fun.data = mean_se,
                                       geom = "errorbar") +
           stat_summary(aes(model, y), fun.y = mean, geom = "point") +
           labs(x = "", y = "Values") +
           coord_flip(),
         "violin" = p + geom_violin(aes(model, y)) +
           stat_summary(aes(model, y), fun.y = mean, geom = "point") +
           labs(x = "", y = "Values") +
           coord_flip()) +
    facet_wrap(~ metric, scales = "free")
}


#' @rdname plot-methods
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- Species ~ .
#' control <- CVControl()
#' 
#' gbmfit <- fit(fo, data = iris, model = GBMModel, control = control)
#' plot(varimp(gbmfit))
#' 
#' gbmres1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbmres2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbmres3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' plot(gbmres3)
#' 
#' res <- Resamples(GBM1 = gbmres1, GBM2 = gbmres2, GBM3 = gbmres3)
#' plot(res)
#' 
plot.Resamples <- function(x, metrics = NULL, stat = base::mean,
                           type = c("boxplot", "density", "errorbar", "violin"),
                           ...) {
  plot(performance(x), metrics = metrics, stat = stat, type = type)
}


#' @rdname plot-methods
#' 
plot.MLModelTune <- function(x, metrics = NULL, stat = base::mean,
                             type = c("boxplot", "density", "errorbar", "line",
                                      "violin"), ...) {
  perf <- x@performance
  type <- match.arg(type)
  if (type == "line") {
    grid <- x@tune_grid
    if (any(dim(grid) == 0)) stop("no tuning parameters to plot")
    stats <- apply(perf, c(3, 2), function(x) stat(na.omit(x))) %>%
      as.data.frame.table
    df <- data.frame(
      x = grid[[1]],
      y = stats$Freq,
      metric = stats$Var2
    )
    
    metriclevels <- levels(df$metric)
    if (is.null(metrics)) {
      metrics <- metriclevels
    } else {
      metrics <- match_indices(metrics, metriclevels)
      df <- df[df$metric %in% metrics, , drop = FALSE]
    }
    df$metric <- factor(df$metric, metrics)
    
    indices <- sapply(grid[-1], function(x) length(unique(x)) > 1)
    args <- list(quote(x), quote(y))
    if (any(indices)) {
      df$group <- interaction(grid[-1][indices])
      args$color <- args$shape <- quote(group)
    }
    mapping <- do.call(aes, args)
    
    ggplot(df, mapping) +
      geom_line() +
      geom_point() +
      labs(x = names(grid)[1], y = "Values", color = "Group",
           shape = "Group") +
      facet_wrap(~ metric, scales = "free")
  } else {
    plot(perf, metrics = metrics, stat = stat, type = type, ...)
  }
}


#' @rdname plot-methods
#' 
#' @param se logical indicating whether to include standard error bars.
#' 
plot.Calibration <- function(x, type = c("line", "point"), se = FALSE, ...) {
  type <- match.arg(type)
  
  args <- list(x = quote(Predicted), y = quote(Mean))
  if (nlevels(x$Response) > 1) args$color <- args$fill <- quote(Response)
  mapping <- do.call(aes,args)

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
      p <- p + geom_ribbon(aes(ymin = Lower, ymax = Upper),
                           linetype = "blank", alpha = 0.2, na.rm = TRUE)
    } else {
      position <- position_dodge(width = 0.025 * Predicted_width)
      p <- p + geom_errorbar(aes(ymin = Lower, ymax = Upper),
                             width = 0.05 * Predicted_width,
                             position = position, na.rm = TRUE)
    }
    
    switch(type,
           "line" = p + geom_line(position = position, na.rm = TRUE),
           "point" = p + geom_point(position = position, na.rm = TRUE))
    
  }, simplify = FALSE)
  
  structure(as(pl, "list"), names = levels(x$Model))
}


#' @rdname plot-methods
#' 
plot.Confusion <- function(x, ...) {
  pl <- list()
  for (name in names(x)) {
    pl[[name]] <- plot(x[[name]]) + labs(title = name)
  }
  pl
}


#' @rdname plot-methods
#' 
plot.ConfusionMatrix <- function(x, ...) {
  df <- as.data.frame(prop.table(x), responseName = "Value")
  df$Predicted <- with(df, factor(Predicted, rev(levels(Predicted))))
  ggplot(df, aes(Observed, Predicted, fill = Value)) +
    geom_raster() +
    labs(fill = "Probability") +
    scale_fill_gradient(trans = "reverse") +
    coord_fixed()
}


#' @rdname plot-methods
#' 
plot.Curves <- function(x, type = c("tradeoffs", "cutoffs"), diagonal = FALSE,
                        stat = base::mean, ...) {
  x <- summary(x, stat = stat)
  
  args <- list(x = quote(x), y = quote(y))
  if (nlevels(x$Model) > 1) args$color <- quote(Model)
  if (!is.null(x$Resample)) args$group <- quote(interaction(Model, Resample))
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
      df <- reshape(x, varying = c("x", "y"), v.names = "y",
                    times = labels, timevar = "Metric",
                    direction = "long")
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
#' @param find numeric true positive rate at which to display reference lines
#' identifying the corresponding rates of positive predictions.
#' 
plot.Lift <- function(x, find = NULL, diagonal = TRUE, stat = base::mean, ...) {
  x <- summary(x, stat = stat)
  p <- plot(Curves(x), diagonal = diagonal, stat = NULL)

  if (!is.null(find)) {
    if (find < 0 || find > 1) warning("'find' rate outside of 0 to 1 range")

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
    df$Resample <- rep(dimnames(tested)$Resample, each = dim(tested)[1])
    
    p <- p +
      geom_segment(aes(x = x, y = 0, xend = x, yend = y), df) +
      geom_segment(aes(x = 0, y = y, xend = x, yend = y), df)
  }
  
  p
}


#' @rdname plot-methods
#' 
#' @param stats vector of numeric indexes or character names of partial
#' dependence summary statistics to plot.
#' 
plot.PartialDependence <- function(x, stats = NULL, ...) {
  if (any(rowSums(!is.na(x$Predictors)) > 1)) {
    stop("partial dependence plots not available for interaction efffects")
  }

  if (!is.null(stats)) {
    stats <- match_indices(stats, levels(x$Statistic))
    x <- x[x$Statistic %in% stats, , drop = FALSE]
  }

  df <- x[c("Statistic", "Response", "Value")]
  
  args <- list(x = quote(Predictor), y = quote(Value))
  if (nlevels(x$Response) > 1) args$color <- quote(Response)
  mapping <- do.call(aes, args)
  
  pl <- list()
  for (varname in names(x$Predictors)) {
    df$Predictor <- x$Predictors[[varname]]
    p <- ggplot(na.omit(df), mapping)
    p <- switch_class(df$Predictor,
                      "factor" = p +
                        geom_crossbar(aes(ymin = ..y.., ymax = ..y..)),
                      "numeric" = p + geom_line() + geom_point()) +
      labs(x = varname) + facet_wrap(~ Statistic, scales = "free")
    pl[[varname]] <- p
  }
  pl
}


#' @rdname plot-methods
#' 
#' @param n number of most important variables to include in the plot
#' [default: all].
#' 
plot.VarImp <- function(x, n = NULL, ...) {
  if (!is.null(n)) x <- head(x, n)
  varnames <- rownames(x)
  df <- cbind(stack(x), variables = factor(varnames, rev(varnames)))
  p <- ggplot(df, aes(variables, values)) +
    geom_bar(stat = "identity") +
    labs(x = "Variables", y = "Importance") +
    coord_flip()
  if (nlevels(df$ind) > 1) p <- p + facet_wrap(~ ind)
  p
}
