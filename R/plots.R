#' Model Performance Plots
#' 
#' Plot measures of model performance and predictor variable importance.
#'  
#' @name plot
#' @rdname plot-method
#' 
#' @param x object to plot.
#' @param metrics vector of numeric indexes or character names of performance
#' metrics to plot.
#' @param stat function to compute a summary statistic on resampled values for
#' \code{MLModelTune} line plots and \code{Resamples} model sorting.
#' @param type type of plot to construct.
#' @param ... arguments passed to other methods.
#' 
#' @seealso \code{\link{diff}}, \code{\link{modelmetrics}}
#' 
plot.ModelMetrics <- function(x, metrics = NULL, stat = mean,
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


#' @rdname plot-method
#' 
#' @seealso \code{\link{resample}}, \code{\link{Resamples}}
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
plot.Resamples <- function(x, metrics = NULL, stat = mean,
                           type = c("boxplot", "density", "errorbar", "violin"),
                           ...) {
  plot(modelmetrics(x), metrics = metrics, stat = stat, type = type)
}


#' @rdname plot-method
#' 
#' @seealso \code{\link{tune}}
#' 
plot.MLModelTune <- function(x, metrics = NULL, stat = mean,
                             type = c("boxplot", "density", "errorbar", "line",
                                      "violin"), ...) {
  resamples <- x@resamples
  type <- match.arg(type)
  if (type == "line") {
    grid <- x@grid
    if (any(dim(grid) == 0)) stop("no tuning parameters to plot")
    stats <- apply(resamples, c(3, 2), function(x) stat(na.omit(x))) %>%
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
    
    mapping <- if (ncol(grid) > 1) {
      df$group <- do.call(interaction, grid[-1])
      aes(x, y, color = group, shape = group)
    } else {
      aes(x, y)
    }
    ggplot(df, mapping) +
      geom_line() +
      geom_point() +
      labs(x = names(grid)[1], y = "Values", color = "Params Group",
           shape = "Params Group") +
      facet_wrap(~ metric, scales = "free")
  } else {
    plot(resamples, metrics = metrics, stat = stat, type = type, ...)
  }
}


#' @rdname plot-method
#' 
#' @param se logical indicating whether to include standard error bars.
#' 
#' @seealso \code{\link{calibration}}
#' 
plot.CalibrationResamples <- function(x, type = c("line", "point"), 
                                      se = FALSE, ...) {
  type <- match.arg(type)
  
  aes_response <- if (nlevels(x$Response) > 1) {
    aes(x = Midpoint, y = Mean, color = Response)
  } else {
    aes(x = Midpoint, y = Mean)
  }
  
  position <- "identity"
  
  pl <- by(x, x$Model, function(cal) {
    
    df <- data.frame(
      Response = cal$Response,
      Midpoint = cal$Midpoint,
      cal$Observed
    )
    Midpoint_width <- diff(range(df$Midpoint))
  
    p <- ggplot(df, aes_response) +
      geom_abline(intercept = 0, slope = 1, color = "gray") +
      labs(title = cal$Model[1], x = "Bin Midpoints", y = "Observed Mean")
    
    if (se) {
      position <- position_dodge(width = 0.025 * Midpoint_width)
      p <- p + geom_errorbar(aes(ymin = Lower, ymax = Upper),
                             width = 0.05 * Midpoint_width,
                             position = position)
    }
  
    switch(type,
           "line" = p + geom_line(position = position),
           "point" = p + geom_point(position = position))
    
  }, simplify = FALSE)
  
  structure(as(pl, "list"), names = levels(x$Model))
}


#' @rdname plot-method
#' 
#' @seealso \code{\link{confusion}}
#' 
plot.ConfusionResamples <- function(x, ...) {
  pl <- list()
  for (name in names(x)) {
    df <- as.data.frame(prop.table(x[[name]]), responseName = "Value")
    df$Predicted <- with(df, factor(Predicted, rev(levels(Predicted))))
    pl[[name]] <- ggplot(df, aes(Observed, Predicted, fill = Value)) +
      geom_raster() +
      labs(title = name, fill = "Probability") +
      scale_fill_gradient(trans = "reverse")
  }
  pl
}


#' @rdname plot-method
#' 
#' @param find numeric percent of observed events at which to display reference
#' lines indicating the corresponding percent tested in lift plots.
#' 
#' @seealso \code{\link{lift}}
#' 
plot.LiftResamples <- function(x, find = NULL, ...) {
  aes_model <- if (nlevels(x$Model) > 1) {
    aes(x = Tested, y = Found, color = Model)
  } else {
    aes(x = Tested, y = Found)
  }

  p <- ggplot(x, aes_model) +
    geom_step() +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    labs(x = "Positive Test Rate (%)",
         y = "True Positive Finding (%)")
  
  if (!is.null(find)) {
    tested <- by(x, x$Model, function(data) {
      interval <- nrow(data) - findInterval(-find, -rev(data$Found)) + 1
      data$Tested[interval]
    })
    df <- data.frame(
      Tested = as.numeric(tested),
      Found = find,
      Model = names(tested)
    )
    p <- p +
      geom_segment(aes(x = Tested, y = 0, xend = Tested, yend = Found), df) +
      geom_segment(aes(x = 0, y = Found, xend = Tested, yend = Found), df)
  }
  
  p
}


#' @rdname plot-method
#' 
#' @param stats vector of numeric indexes or character names of partial
#' dependence summary statistics to plot.
#' 
#' @seealso \code{\link{dependence}}
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

  aes_response <- if (nlevels(x$Response) > 1) {
    aes(x = Predictor, y = Value, color = Response)
  } else {
    aes(x = Predictor, y = Value)
  } 

  pl <- list()
  for (varname in names(x$Predictors)) {
    df$Predictor <- x$Predictors[[varname]]
    p <- ggplot(na.omit(df), aes_response)
    p <- switch_class(df$Predictor,
                      "factor" = p +
                        geom_crossbar(aes(ymin = ..y.., ymax = ..y..)),
                      "numeric" = p + geom_line() + geom_point()) +
      labs(x = varname) + facet_wrap(~ Statistic, scales = "free")
    pl[[varname]] <- p
  }
  pl
}


#' @rdname plot-method
#' 
#' @param n number of most important variables to include in the plot
#' [default: all].
#' 
#' @seealso \code{\link{varimp}}
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
