PartialDependence <- function(object) {
  structure(object, class = c("PartialDependence", "data.frame"))
}


#' Partial Dependence
#' 
#' Calculate partial dependence of a response on select predictor variables.
#' 
#' @param object \code{MLModelFit} object.
#' @param data \code{data.frame} containing all predictor variables.  If not
#' specified, the training data will be used by default.
#' @param select expression indicating predictor variables for which to compute
#' partial dependence (see \code{\link[base]{subset}} for syntax)
#' [default: all].
#' @param interaction logical indicating whether to calculate dependence on the
#' interacted predictors.
#' @param n number of predictor values at which to perform calculations.
#' @param intervals character string specifying whether the \code{n} values are
#' spaced uniformly (\code{"uniform"}) or according to variable quantiles
#' (\code{"quantile"}).
#' @param stats function, one or more function names, or list of named functions
#' with which to aggregate the response variable over the non-selected predictor
#' variables.
#' 
#' @return \code{PartialDependence} class object that inherits from
#' \code{data.frame}.
#'  
#' @seealso \code{\link{fit}}, \code{\link{plot}}
#' 
#' @examples
#' gbmfit <- fit(Species ~ ., data = iris, model = GBMModel)
#' (pd <- dependence(gbmfit, select = c(Petal.Length, Petal.Width)))
#' plot(pd)
#' 
dependence <- function(object, data = NULL, select = NULL, interaction = FALSE,
                       n = 10, intervals = c("uniform", "quantile"),
                       stats = c("Mean" = base::mean)) {
  
  stopifnot(is(object, "MLModelFit"))

  x <- fitbit(object, "x")
  if (is.null(data)) data <- getdata(x)

  x_vars <- all.vars(delete.response(terms(x)))
  indices <- structure(match(x_vars, names(data)), names = x_vars)
  select <- eval(substitute(select), as.list(indices), parent.frame())
  if (is.null(select)) select <- indices
  data_select <- data[, select, drop = FALSE]
  
  intervals <- match.arg(intervals)
  
  stats <- list2function(stats)

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
  
  grid_list <- lapply(data_select, select_values)
  
  data_select_grid <- if (interaction) {
    expand.grid(grid_list)
  } else {
    df <- data.frame(row.names = 1:sum(sapply(grid_list, length)))
    pos <- 0
    for (name in names(grid_list)) {
      n <- length(grid_list[[name]])
      df[[name]] <- rep(grid_list[[name]], length.out = nrow(df))
      df[[name]][-(pos + seq_len(n))] <- NA
      pos <- pos + n
    }
    df
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
