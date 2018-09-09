#' Model Performance Plots
#' 
#' Plot measures of model performance and predictor variable importance.
#'  
#' @name plot
#' @rdname plot-method
#' 
#' @param x object to plot.
#' @param metrics vector of numeric indexes or character names of the performance
#' metrics to plot.
#' @param stat function to compute a summary statistic on resampled values for
#' MLModelTune line plots and Resamples model sorting.  The supplied function
#' should contain a \code{na.rm} argument in its definition.
#' @param type type of plot to construct.
#' @param ... arguments to be passed to other methods.
#' 
#' @seealso \code{\link{resample}}, \code{\link{Resamples}}, \code{\link{tune}}
#' 
plot.MLModelTune <- function(x, metrics = NULL, stat = mean,
                             type = c("boxplot", "density", "errorbar", "line",
                                      "violin"), ...) {
  resamples <- x@resamples
  type <- match.arg(type)
  if(type == "line") {
    grid <- x@grid
    if(any(dim(grid) == 0)) stop("no tuning parameters to plot")
    stats <- as.data.frame.table(apply(resamples, c(3, 2), stat, na.rm = TRUE))
    df <- data.frame(
      x = grid[[1]],
      y = stats$Freq,
      metric = stats$Var2
    )
    
    metriclevels <- levels(df$metric)
    if(is.null(metrics)) {
      metrics <- metriclevels
    } else {
      metrics <- match.indices(metrics, metriclevels)
      df <- df[df$metric %in% metrics, , drop = FALSE]
    }
    df$metric <- factor(df$metric, metrics)
    
    mapping <- if(ncol(grid) > 1) {
      df$group <- do.call(interaction, grid[-1])
      aes(x, y, color = group, shape = group)
    } else {
      aes(x, y)
    }
    p <- ggplot(df, mapping) +
      geom_line() +
      geom_point() +
      labs(x = names(grid)[1], y = "Values", color = "Params Group",
           shape = "Params Group")
    if(nlevels(df$metric) > 1) p <- p + facet_wrap(~ metric, scales = "free")
    p
  } else {
    plot(resamples, metrics = metrics, stat = stat, type = type, ...)
  }
}


#' @rdname plot-method
#' 
plot.Resamples <- function(x, metrics = NULL, stat = mean,
                           type = c("boxplot", "density", "errorbar", "violin"),
                           ...) {
  df <- as.data.frame.table(x)
  if(length(dim(x)) <= 2) df$Var3 <- factor("Model")
  orderednames <- match(c("Var1", "Var2", "Var3", "Freq"), names(df))
  names(df)[orderednames] <- c("resample", "metric", "model", "y")
  
  metriclevels <- levels(df$metric)
  if(is.null(metrics)) {
    metrics <- metriclevels
  } else {
    metrics <- match.indices(metrics, metriclevels)
    df <- df[df$metric %in% metrics, , drop = FALSE]
  }
  df$metric <- factor(df$metric, metrics)
  
  firstmetric <- df[df$metric == metrics[1], , drop = FALSE]
  sortedlevels <- tapply(firstmetric$y, firstmetric$model, stat,
                         na.rm = TRUE) %>% sort %>% names
  df$model <- factor(df$model, sortedlevels)
  
  p <- ggplot(df)
  p <- switch(match.arg(type),
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
                coord_flip())
  if(nlevels(df$metric) > 1) p <- p + facet_wrap(~ metric, scales = "free")
  p
}


#' @rdname plot-method
#' 
#' @param n number of most important variables to include in the plot
#' (default: all).
#' 
#' @seealso \code{\link{varimp}}
#' 
plot.VarImp <- function(x, n = NULL, ...) {
  if(!is.null(n)) x <- head(x, n)
  varnames <- rownames(x)
  df <- cbind(stack(x), variables = factor(varnames, rev(varnames)))
  p <- ggplot(df, aes(variables, values)) +
    geom_bar(stat = "identity") +
    labs(x = "Variables", y = "Importance") +
    coord_flip()
  if(nlevels(df$ind) > 1) p <- p + facet_wrap(~ ind)
  p
}
