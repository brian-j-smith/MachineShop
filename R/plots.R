#' Variable Importance Plot
#' 
#' Plot measures of the relative importance of predictors in a model.
#' 
#' @param x variable importance object as returned by \code{\link{varimp}}.
#' @param n number of most important variables to include in the plot
#' (default: all).
#' @param ... arguments to be passed to other methods.
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
