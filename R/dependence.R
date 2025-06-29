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
#' @param distr,method arguments passed to \code{\link{predict}}.
#' @param stats function, function name, or vector of these with which to
#'   compute response variable summary statistics over non-selected predictor
#'   variables.
#' @param na.rm logical indicating whether to exclude missing predicted response
#'   values from the calculation of summary statistics.
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
  intervals = c("uniform", "quantile"), distr = character(),
  method = character(),
  stats = MachineShop::settings("stats.PartialDependence"), na.rm = TRUE
) {

  stopifnot(is(object, "MLModelFit"))

  input <- as.MLInput(object)
  if (is.null(data)) data <- input
  data <- as.data.frame(data)
  pred_names <- all.vars(predictors(terms(input, original = TRUE)))
  pred_names <- do.call(
    subset_names, list(pred_names, substitute(select)), envir = parent.frame()
  )

  data_select <- data[, pred_names, drop = FALSE]

  intervals <- match.arg(intervals)

  stats <- check_stats(stats, convert = TRUE)
  throw(check_assignment(stats))

  select_values <- function(x) {
    if (is(x, "factor")) {
      unique(x)
    } else if (is(x, "logical")) {
      intersect(c(FALSE, TRUE), x)
    } else if (is(x, "numeric") || is(x, "Date")) {
      x <- sort(x)
      n <- min(n, length(x))
      unique(switch(intervals,
        "quantile" = x[round(seq(1, length(x), length = n))],
        "uniform" = {
          y <- seq(x[1], x[length(x)], length = n)
          indices <- findInterval(y, x, all.inside = TRUE)
          x_lower <- x[indices]
          x_upper <- x[indices + 1]
          ifelse(y - x_lower < x_upper - y, x_lower, x_upper)
        }
      ))
    } else {
      throw(TypeError(
        x, c("Date", "factor", "logical", "numeric"), "selected variable"
      ))
    }
  }

  predict_stats <- function(data) {
    stats_list <- map(
      function(x) stats(if (na.rm) na.omit(x) else x),
      as.data.frame(
        predict(
          object, newdata = data, type = "raw", distr = distr, method = method
        )
      )
    )
    x <- do.call(cbind, stats_list)
    if (is.null(rownames(x))) rownames(x) <- make_unique(rep("stat", nrow(x)))
    if (is.null(colnames(x))) colnames(x) <- make_unique(rep("y", ncol(x)))
    names(dimnames(x)) <- c("Statistic", "Response")
    as.data.frame(TabularArray(x))
  }

  grid_list <- map(select_values, data_select)

  data_select_grid <- if (interaction) {
    expand.grid(grid_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  } else {
    df <- data.frame(row.names = seq_len(sum(lengths(grid_list))))
    start <- 1
    for (name in names(grid_list)) {
      n <- length(grid_list[[name]])
      df[[name]] <- rep_len(grid_list[[name]], nrow(df))
      df[[name]][-seq(start, length = n)] <- NA
      start <- start + n
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
