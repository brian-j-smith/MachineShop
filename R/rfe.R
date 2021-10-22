#' Recursive Feature Elimination
#'
#' A wrapper method of backward feature selection in which a given model is fit
#' to nested subsets of most important predictor variables in order to select
#' the subset whose resampled predictive performance is optimal.
#'
#' @name rfe
#' @rdname rfe-methods
#'
#' @param ... arguments passed from the generic function to its methods and from
#'   the \code{MLModel} and \code{MLModelFunction} methods to others.  The
#'   first arguments of \code{rfe} methods are positional and, as such, must be
#'   given first in calls to them.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param input \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param model \link[=models]{model} function, function name, or object.  Can
#'   be given first followed by any of the variable specifications, and can be
#'   omitted in the case of \link[=ModeledInput]{modeled inputs}.
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param props numeric vector of the proportions of most important predictor
#'   variables to retain in fitted models or an integer number of equal spaced
#'   proportions to generate automatically; ignored if \code{sizes} are given.
#' @param sizes integer vector of the set sizes of most important predictor
#'   variables to retain.
#' @param recompute logical indicating whether to recompute variable importance
#'   after eliminating each set of variables.
#' @param optimize character string specifying a search through all \code{props}
#'   to identify the globally optimal model (\code{"global"}) or a search that
#'   stops after identifying the first locally optimal model (\code{"local"}).
#' @param samples numeric vector or list giving the number of permutation
#'   samples for each of the \code{rfe} and \code{\link{varimp}} algorithms.
#'   One or both of the values may be specified as named arguments or in the
#'   order in which their defaults appear.  Larger numbers of samples decrease
#'   variability in estimated model performances and variable importances at the
#'   expense of increased computation time.  Samples are more expensive
#'   computationally for \code{rfe} than for \code{varimp}.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values and permuted samples.
#'
#' @return A data frame with columns for the numbers of predictor variables
#' retained (size), their names (terms), logical indicators to identify the
#' optimal model (optimal), and associated predictive performances
#' (performance).
#'
#' @seealso \code{\link{varimp}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' rfe(sale_amount ~ ., data = ICHomes, model = GBMModel)
#' }
#'
rfe <- function(...) {
  UseMethod("rfe")
}


#' @rdname rfe-methods
#'
rfe.formula <- function(
  formula, data, model, control = MachineShop::settings("control"), props = 4,
  sizes = NULL, recompute = FALSE, optimize = c("global", "local"),
  samples = c(rfe = 1, varimp = 1), metrics = NULL, stat = "base::mean", ...
) {
  args <- list(formula, data, strata = response(formula), na.rm = FALSE)
  mf <- do.call(ModelFrame, args)
  rfe(mf, model, control, props = props, sizes = sizes, recompute = recompute,
      optimize = optimize, samples = samples, metrics = metrics, stat = stat)
}


#' @rdname rfe-methods
#'
rfe.matrix <- function(
  x, y, model, control = MachineShop::settings("control"), props = 4,
  sizes = NULL, recompute = FALSE, optimize = c("global", "local"),
  samples = c(rfe = 1, varimp = 1), metrics = NULL, stat = "base::mean", ...
) {
  mf <- ModelFrame(x, y, strata = y, na.rm = FALSE)
  rfe(mf, model, control, props = props, sizes = sizes, recompute = recompute,
      optimize = optimize, samples = samples, metrics = metrics, stat = stat)
}


#' @rdname rfe-methods
#'
rfe.ModelFrame <- function(
  input, model, control = MachineShop::settings("control"), props = 4,
  sizes = NULL, recompute = FALSE, optimize = c("global", "local"),
  samples = c(rfe = 1, varimp = 1), metrics = NULL, stat = "base::mean", ...
) {
  update <- function(input, data) {
    if (isS4(input)) input@.Data <- data else input[] <- data
    input
  }
  .rfe(input, update, model, control, props = props, sizes = sizes,
       recompute = recompute, optimize = match.arg(optimize), samples = samples,
       metrics = metrics, stat = stat)
}


#' @rdname rfe-methods
#'
rfe.recipe <- function(
  input, model, control = MachineShop::settings("control"), props = 4,
  sizes = NULL, recompute = FALSE, optimize = c("global", "local"),
  samples = c(rfe = 1, varimp = 1), metrics = NULL, stat = "base::mean", ...
) {
  update <- function(input, data) recipe(input, data)
  .rfe(ModelRecipe(input), update, model, control, props = props,
       sizes = sizes, recompute = recompute, optimize = match.arg(optimize),
       samples = samples, metrics = metrics, stat = stat)
}


#' @rdname rfe-methods
#'
rfe.MLModel <- function(model, ...) {
  rfe(..., model = model)
}


#' @rdname rfe-methods
#'
rfe.MLModelFunction <- function(model, ...) {
  rfe(model(), ...)
}


.rfe <- function(
  input, update, model, control, props, sizes, recompute, optimize, samples,
  metrics, stat
) {
  data <- as.data.frame(input)
  model_fit <- fit(input, model)
  control <- get_MLControl(control)

  get_samples <- function(rfe = 1, varimp = 1) as.list(environment())
  samples <- do.call("get_samples", as.list(samples))
  samples$rfe <- check_integer(samples$rfe, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples$rfe))
  inds <- replicate(samples$rfe, permute_int(nrow(data))$j)

  times <- control@predict$times
  if (is.null(metrics)) {
    obs <- response(model_fit)
    pred <- predict(model_fit, times = times, type = "prob")
    metrics <- get_perf_metrics(obs, pred)
  }
  metric <- check_metric(c(metrics)[[1]], convert = TRUE)
  throw(check_assignment(metrics, metric))
  loss <- function(x) if (metric@maximize) -x[, 1] else x[, 1]
  stat <- check_stat(stat, convert = TRUE)
  throw(check_assignment(stat))
  apply_stat <- function(x) {
    apply(do.call(cbind, x), 1, function(x) stat(na.omit(x)))
  }

  varimp <- function(object, ...) {
    MachineShop::varimp(
      object, scale = FALSE, method = "permute", samples = samples$varimp,
      times = times, metric = metric, stats = stat, ...
    )
  }
  vi_names <- rownames(varimp(model_fit))
  superset <- vi_names

  if (!is.null(sizes)) {
    sizes <- check_integer(sizes, bounds = c(1, Inf), size = NA)
    throw(check_assignment(sizes))
    sizes <- pmin(sizes, length(superset))
  } else {
    nprops <- check_integer(props, bounds = c(1, Inf), size = 1)
    if (is(nprops, "error")) {
      props <- check_numeric(props, bounds = c(0, 1), include = 0:1, size = NA)
      throw(check_assignment(props))
    } else {
      props <- seq(0, 1, length = nprops + 1)[-1]
    }
    sizes <- pmax(1, round(props * length(superset)))
  }
  sizes <- sort(unique(sizes), decreasing = TRUE)

  pb <- progress_bar$new(
    format = "rfe [:bar] :percent | :eta",
    total = length(sizes) * samples$rfe
  )
  on.exit(pb$terminate())

  subsets <- list()
  perfs <- NULL
  for (size in sizes) {

    subset <- head(vi_names, size)
    drop <- setdiff(superset, subset)

    perf_samples <- list()
    vi_samples <- list()
    for (s in seq_len(samples$rfe)) {

      data[drop] <- data[inds[, s], drop]
      input <- update(input, data)
      res <- resample(input, model, control)
      perf_samples[[s]] <- summary(
        performance(res, metrics = metrics),
        stats = stat
      )[, 1, drop = FALSE]

      if (recompute && size > tail(sizes, 1)) {
        vi <- do.call(varimp, list(fit(input, model), select = subset))
        vi_samples[[s]] <- vi[subset, 1, drop = FALSE]
      }

      pb$tick()

    }

    if (length(vi_samples)) {
      vi_names <- names(sort(apply_stat(vi_samples), decreasing = TRUE))
    }

    subsets <- c(subsets, list(subset))
    perfs <- rbind(perfs, apply_stat(perf_samples))

    check_local_perfs <- optimize == "local" && nrow(perfs) > 1
    if (check_local_perfs && diff(tail(loss(perfs), 2)) > 0) break

  }

  tbl <- tibble(
    size = lengths(subsets),
    terms = subsets,
    optimal = FALSE,
    performance = perfs
  )
  tbl$optimal[which.min(loss(perfs))] <- TRUE
  tbl
}
