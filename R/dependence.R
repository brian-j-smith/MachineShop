#' Partial Dependence
#' 
#' Calculate partial dependence of a response on select predictor variables.
#' 
#' @param object MLModelFit object.
#' @param data data frame containing all predictor variables.
#' @param select expression indicating predictor variables for which to compute
#' partial dependence (see \code{\link[base]{subset}} for syntax).
#' @param interaction logical indicating whether to calculate dependence on the
#' interacted predictors.
#' @param n number of predictor values at which to perform calculations.
#' @param intervals character string specifying whether the \code{n} values are
#' spaced according to variable quantiles (\code{"quantile"}) or uniformly
#' (\code{"uniform"}).
#' @param stats function or list of named functions with which to aggregate
#' the response variable over the non-selected predictor variables.
#' 
#' @return PartialDependence class object.
#'  
#' @seealso \code{\link{fit}}, \code{\link{plot}}
#' 
#' @examples
#' gbmfit <- fit(factor(Species) ~ ., data = iris, model = GBMModel)
#' (pd <- dependence(gbmfit, data = iris, select = c(Petal.Length, Petal.Width)))
#' plot(pd)
#' 
dependence <- function(object, data, select, interaction = FALSE, n = 10,
                       intervals = c("uniform", "quantile"),
                       stats = c("Mean" = mean)) {

  intervals <- match.arg(intervals)
  
  if (is.list(stats)) {
    stats <- eval(bquote(
      function(x) sapply(.(stats), function(stat) stat(x))
    ))
  }
  
  select_values <- function(x) {
    if (is.factor(x)) {
      unique(x)
    } else if (is.vector(x)) {
      switch(intervals,
             "quantile" = quantile(x, seq(0, 1, length = n)),
             "uniform" = seq(min(x), max(x), length = n)
      )
    } else {
      stop("unsupported variable type")
    }
  }
  
  predict_stats <- function(data) {
    stats_list <- predict(object, newdata = data, type = "prob")  %>%
      as.data.frame %>%
      as.list %>%
      lapply(stats)
    x <- do.call(cbind, stats_list)
    if (is.null(rownames(x))) rownames(x) <- make.unique(rep("stat", nrow(x)))
    if (is.null(colnames(x))) colnames(x) <- make.unique(rep("y", ncol(x)))
    names(dimnames(x)) <- c("Statistic", "Response")
    as.data.frame.table(x, responseName = "Value")
  }
  
  data_select <- eval(substitute(subset(data, select = select)))
  grid_list <- lapply(data_select, select_values)
  
  data_select_grid <- if (interaction) {
    do.call(expand.grid, grid_list)
  } else {
    grid_list <- lapply(seq_len(length(grid_list)), function(i) {
      df <- as.data.frame(grid_list[i])
      df[names(grid_list)[-i]] <- NA
      df
    })
    do.call(rbind, grid_list)
  }

  dependence_list <- lapply(seq_len(nrow(data_select_grid)), function(i) {
    for (varname in names(data_select_grid)) {
      x <- data_select_grid[i, varname]
      if (!is.na(x)) data[[varname]] <- x
    }
    df <- predict_stats(data)
    df$Predictors <- data_select_grid[rep(i, nrow(df)), , drop = FALSE]
    df
  })
  
  PartialDependence(do.call(rbind, dependence_list))

}
