#' Model Performance Plot
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
#' sorting of the plotted models.  The supplied function should contain a
#' \code{na.rm} argument in its definition.
#' @param type type of plot to construct.
#' @param ... arguments to be passed to other methods.
#' 
#' @seealso \code{\link{resample}}, \code{\link{Resamples}}, \code{\link{tune}}
#' 
plot.MLModelTune <- function(x, metrics = NULL, stat = mean,
                           type = c("boxplot", "violin", "errorbar"), ...) {
  plot(x@resamples, metrics = metrics, stat = stat, type = type, ...)
}


#' @rdname plot-method
#' 
plot.Resamples <- function(x, metrics = NULL, stat = mean,
                           type = c("boxplot", "violin", "errorbar"), ...) {
  df <- as.data.frame.table(x)
  if(length(dim(x)) <= 2) df$Var3 <- factor("Model")
  lookup <- structure(1:nlevels(df$Var2), names = levels(df$Var2))
  if(!is.null(metrics)) {
    metrics <- na.omit(names(lookup)[lookup[metrics]])
    if(length(metrics) == 0) {
      metrics <- names(lookup)[1]
      warning("specified metrics not found; plotting ", metrics, " instead")
    }
    df <- df[df$Var2 %in% metrics, , drop = FALSE]
  } else {
    metrics <- names(lookup)
  }
  firstmetric <- df[df$Var2 == metrics[1], , drop = FALSE]
  sortedlevels <- tapply(firstmetric$Freq, firstmetric$Var3, stat,
                         na.rm = TRUE) %>% sort %>% names
  df$Var3 <- factor(df$Var3, sortedlevels)
  p <- ggplot(df, aes(Var3, Freq))
  p <- switch(match.arg(type),
              "boxplot" = p + geom_boxplot(),
              "violin" = p + geom_violin(),
              "errorbar" = p + stat_summary(fun.data = mean_se,
                                            geom = "errorbar"))
  p <- p + stat_summary(fun.y = mean, geom = "point") +
        labs(x = "", y = "Values") +
        coord_flip()
  if(nlevels(df$Var2) > 1) p <- p + facet_wrap(~ Var2, scales = "free")
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
