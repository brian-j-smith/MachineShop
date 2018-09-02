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
