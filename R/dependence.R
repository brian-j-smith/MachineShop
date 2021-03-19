PartialDependence <- function(object) {
  structure(object, class = c("PartialDependence", "data.frame"))
}


#' Partial Dependence
#'
#' Calculate partial dependence of a response on select predictor variables.
#'
#' @param object model \link{fit} result.
#' @param data \link[=data.frame]{data frame} containing all predictor
#'   variables.  If not specified, the training data will be used by default.
#' @param select expression indicating predictor variables for which to compute
#'   partial dependence (see \code{\link[base]{subset}} for syntax)
#'   [default: all].
#' @param interaction logical indicating whether to calculate dependence on the
#'   interacted predictors.
#' @param n number of predictor values at which to perform calculations.
#' @param intervals character string specifying whether the \code{n} values are
#'   spaced uniformly (\code{"uniform"}) or according to variable quantiles
#'   (\code{"quantile"}).
#' @param stats function, function name, or vector of these with which to
#'   compute response variable summary statistics over non-selected predictor
#'   variables.
#'
#' @return \code{PartialDependence} class object that inherits from
#' \code{data.frame}.
#'
#' @seealso \code{\link{plot}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' gbm_fit <- fit(Species ~ ., data = iris, model = GBMModel)
#' (pd <- dependence(gbm_fit, select = c(Petal.Length, Petal.Width)))
#' plot(pd)
#' }
#'
dependence <- function(
  object, data = NULL, select = NULL, interaction = FALSE, n = 10,
  intervals = c("uniform", "quantile"),
  stats = MachineShop::settings("stats.PartialDependence")
) {

  stopifnot(is(object, "MLModelFit"))

  x <- as.MLModel(object)@x
  if (is.null(data)) data <- x
  data <- as.data.frame(data)
  vars <- all.vars(predictors(terms(x, original = TRUE)))

  indices <- structure(match(vars, names(data)), names = vars)
  select <- eval(substitute(select), as.list(indices), parent.frame())
  if (is.null(select)) select <- indices
  data_select <- data[, select, drop = FALSE]

  intervals <- match.arg(intervals)

  stats <- list_to_function(stats)

  select_values <- function(x) {
    if (is.factor(x)) {
      unique(x)
    } else if (is.vector(x)) {
      x <- sort(x)
      n <- min(n, length(x))
      switch(intervals,
        "quantile" = x[round(seq(1, length(x), length = n))],
        "uniform" = {
          y <- seq(x[1], x[length(x)], length = n)
          indices <- findInterval(y, x, all.inside = TRUE)
          x_lower <- x[indices]
          x_upper <- x[indices + 1]
          unique(ifelse(y - x_lower < x_upper - y, x_lower, x_upper))
        }
      )
    } else {
      stop("unsupported variable type")
    }
  }

  predict_stats <- function(data) {
    stats_list <- predict(object, newdata = data, type = "prob")  %>%
      as.data.frame %>%
      as.list %>%
      map(stats, .)
    x <- do.call(cbind, stats_list)
    if (is.null(rownames(x))) rownames(x) <- make.unique(rep("stat", nrow(x)))
    if (is.null(colnames(x))) colnames(x) <- make.unique(rep("y", ncol(x)))
    names(dimnames(x)) <- c("Statistic", "Response")
    as.data.frame(TabularArray(x))
  }

  grid_list <- map(select_values, data_select)

  data_select_grid <- if (interaction) {
    expand.grid(grid_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  } else {
    df <- data.frame(row.names = 1:sum(lengths(grid_list)))
    pos <- 0
    for (name in names(grid_list)) {
      n <- length(grid_list[[name]])
      df[[name]] <- rep_len(grid_list[[name]], nrow(df))
      df[[name]][-(pos + seq_len(n))] <- NA
      pos <- pos + n
    }
    df
  }

  dependence_list <- map(function(i) {
    for (varname in names(data_select_grid)) {
      x <- data_select_grid[i, varname]
      if (!is.na(x)) data[[varname]] <- x
    }
    df <- predict_stats(data)
    df$Predictors <- data_select_grid[rep(i, nrow(df)), , drop = FALSE]
    df
  }, seq_len(nrow(data_select_grid)))

  PartialDependence(do.call(rbind, dependence_list))

}
