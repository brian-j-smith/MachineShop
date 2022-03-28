#' Recursive Feature Elimination
#'
#' A wrapper method of backward feature selection in which a given model is fit
#' to nested subsets of most important predictor variables in order to select
#' the subset whose resampled predictive performance is optimal.
#'
#' @name rfe
#' @rdname rfe-methods
#'
#' @param ... arguments passed from the \code{MLModel} and
#'   \code{MLModelFunction} methods to others and from the others to
#'   \code{ModelSpecification}.  The first argument of each \code{rfe}
#'   method is positional and, as such, must be given first in calls to them.
#' @param object model \link[=inputs]{input} or
#'   \link[=ModelSpecification]{specification}.
#' @param formula,data \link[=formula]{formula} defining the model predictor and
#'   response variables and a \link[=data.frame]{data frame} containing them.
#' @param x,y \link{matrix} and object containing predictor and response
#'   variables.
#' @param input \link[=inputs]{input} object defining and containing the model
#'   predictor and response variables.
#' @param model \link[=models]{model} function, function name, or object; or
#'   another object that can be \link[=as.MLModel]{coerced} to a model.  A model
#'   can be given first followed by any of the variable specifications, and the
#'   argument can be omitted altogether in the case of
#'   \link[=ModeledInput]{modeled inputs}.
#' @param select expression indicating predictor variables that can be
#'   eliminated (see \code{\link[base]{subset}} for syntax) [default: all].
#' @param control \link[=controls]{control} function, function name, or object
#'   defining the resampling method to be employed.
#' @param props numeric vector of the proportions of most important predictor
#'   variables to retain in fitted models or an integer number of equal spaced
#'   proportions to generate automatically; ignored if \code{sizes} are given.
#' @param sizes integer vector of the set sizes of most important predictor
#'   variables to retain.
#' @param random logical indicating whether to eliminate variables at random
#'   with probabilities proportional to their importance.
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
#' @param stat functions or character strings naming functions to compute
#'   summary statistics on resampled metric values and permuted samples.  One or
#'   both of the values may be specified as named arguments or in the order in
#'   which their defaults appear.
#' @param progress logical indicating whether to display iterative progress
#'   during elimination.
#'
#' @return \code{TrainingStep} class object containing a summary of the numbers
#' of predictor variables retained (size), their names (terms), logical
#' indicators for the optimal model selected (selected), and associated
#' performance metrics (metrics).
#'
#' @seealso \code{\link{performance}}, \code{\link{plot}},
#' \code{\link{summary}}, \code{\link{varimp}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' (res <- rfe(sale_amount ~ ., data = ICHomes, model = GBMModel))
#' summary(res)
#' summary(performance(res))
#' plot(res, type = "line")
#' }
#'
rfe <- function(...) {
  UseMethod("rfe")
}


#' @rdname rfe-methods
#'
rfe.formula <- function(formula, data, model, ...) {
  modelspec <- ModelSpecification(formula, data, model = model, control = NULL)
  rfe(modelspec, ..., .envir = parent.frame())
}


#' @rdname rfe-methods
#'
rfe.matrix <- function(x, y, model, ...) {
  modelspec <- ModelSpecification(x, y, model = model, control = NULL)
  rfe(modelspec, ..., .envir = parent.frame())
}


#' @rdname rfe-methods
#'
rfe.ModelFrame <- function(input, model = NULL, ...) {
  modelspec <- ModelSpecification(input, model = model, control = NULL)
  rfe(modelspec, ..., .envir = parent.frame())
}


#' @rdname rfe-methods
#'
rfe.recipe <- function(input, model = NULL, ...
) {
  modelspec <- ModelSpecification(input, model = model, control = NULL)
  rfe(modelspec, ..., .envir = parent.frame())
}


#' @rdname rfe-methods
#'
rfe.ModelSpecification <- function(
  object, select = NULL, control = MachineShop::settings("control"), props = 4,
  sizes = integer(), random = FALSE, recompute = TRUE,
  optimize = c("global", "local"), samples = c(rfe = 1, varimp = 1),
  metrics = NULL,
  stat = c(
    resample = MachineShop::settings("stat.Resample"),
    permute = MachineShop::settings("stat.TrainingParams")
  ), progress = FALSE, ...
) {

  data <- as.data.frame(object)
  model_fit <- fit(object)
  control <- as.MLControl(control)
  optimize <- match.arg(optimize)

  method <- paste(optimize, "Recursive Feature Elimination")
  substring(method, 1, 1) <- toupper(substring(method, 1, 1))
  progress <- if (throw(check_logical(progress))) {
    heading(paste(class(object), "feature selection"))
    new_print_progress()
  } else {
    function(...) NULL
  }

  get_samples_args <- function() as.list(environment())
  formals(get_samples_args) <- eval(formals(rfe.ModelSpecification)$samples)
  samples <- do.call("get_samples_args", as.list(samples))
  samples$rfe <- check_integer(samples$rfe, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples$rfe))
  inds <- replicate(samples$rfe, permute_int(nrow(data))$j)

  times <- control@predict$times
  if (is.null(metrics)) {
    obs <- response(model_fit)
    pred <- predict(model_fit, times = times, type = "default")
    metrics <- get_perf_metrics(obs, pred)
  }
  metric <- check_metric(c(metrics)[[1]], convert = TRUE)
  throw(check_assignment(metrics, metric))
  loss <- function(x) if (metric@maximize) -x[, 1] else x[, 1]

  get_stat_args <- function() as.list(environment())
  formals(get_stat_args) <- eval(formals(rfe.ModelSpecification)$stat)
  stat <- do.call("get_stat_args", as.list(c(stat)))
  stat$resample <- check_stat(stat$resample, convert = TRUE)
  throw(check_assignment(stat$resample))
  stat$permute <- check_stat(stat$permute, convert = TRUE)
  throw(check_assignment(stat$permute))
  apply_stat_permute <- function(x, along) {
    x <- do.call(abind, c(x, along = along))
    margins <- setdiff(seq_len(ndim(x)), along)
    apply(x, margins, function(x) stat$permute(na.omit(x)))
  }

  varimp <- function(object, select = NULL) {
    res <- do.call(MachineShop::varimp, list(
      object, scale = FALSE, method = "permute", select = select,
      samples = samples$varimp, times = times, metric = metric, stats = stat,
      progress = !is.null(body(progress))
    ))
    structure(res[[1]], names = rownames(res))
  }
  scale <- function(x) {
    scale <- max(x, 0)
    if (scale > 0) x <- x / scale
    pmin(pmax(sort(x, decreasing = TRUE), 0.01), 0.99)
  }

  keep <- character()
  superset <- all.vars(predictors(terms(as.MLInput(object), original = TRUE)))
  if (!missing(select)) {
    .envir <- list(...)$.envir
    if (is.null(.envir)) .envir <- parent.frame()
    select <- do.call(subset_names, list(superset, substitute(select)),
                      envir = .envir)
    keep <- setdiff(superset, select)
    superset <- select
  }
  vi <- scale(varimp(model_fit, select = superset))

  if (length(sizes)) {
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
    sizes <- pmax(round(props * length(superset)), 1)
  }
  sizes <- sort(unique(sizes), decreasing = TRUE)

  subsets <- list()
  perf_list <- list()
  perf_stats <- NULL
  scores <- numeric()

  max_iter <- length(sizes)
  for (i in seq_len(max_iter)) {
    size <- sizes[i]
    if (random) {
      subset <- sample(names(vi), size, prob = vi)
      subset <- subset[order(vi[subset], decreasing = TRUE)]
    } else {
      subset <- head(names(vi), size)
    }
    drop <- setdiff(superset, subset)

    perf_samples <- list()
    vi_samples <- list()
    for (s in seq_len(samples$rfe)) {
      data[drop] <- data[inds[, s], drop]
      object <- update(object, data = data)
      res <- resample(object, control = control)
      perf_samples[[s]] <- performance(res, metrics = metrics)

      if (recompute && size > tail(sizes, 1)) {
        vi <- varimp(fit(object), select = subset)
        vi_samples[[s]] <- vi[subset]
      }
    }

    vi <- if (length(vi_samples)) {
      scale(apply_stat_permute(vi_samples, 2))
    } else {
      vi[subset]
    }

    subsets[[i]] <- c(keep, subset)
    perf <- perf_samples[[1]]
    if (s > 1) S3Part(perf) <- apply_stat_permute(perf_samples, 3)
    perf_list[[i]] <- perf
    perf_stats <- rbind(
      perf_stats,
      t(summary(perf, stat = stat$resample)[, 1, drop = FALSE])
    )
    scores[i] <- ifelse(metric@maximize, 1, -1) * perf_stats[i, 1]

    progress(
      scores, max_iter = max_iter, method = method,
      items = list("Term{?s}: " = subset),
      metric = structure(perf_stats[i, 1], names = metric@label)
    )

    check_local_perfs <- optimize == "local" && i > 1
    if (check_local_perfs && diff(tail(scores, 2)) < 0) break
  }

  names(perf_list) <- make_unique(rep_len(class(object), length(perf_list)))

  TrainingStep(
    object,
    method = method,
    names = names(perf_list),
    items = tibble(terms = subsets),
    params = tibble(size = lengths(subsets)),
    metrics = perf_stats,
    selected = which.max(scores),
    performance = do.call(c, perf_list)
  )

}


#' @rdname rfe-methods
#'
rfe.MLModel <- function(model, ...) {
  rfe(..., model = model)
}


#' @rdname rfe-methods
#'
rfe.MLModelFunction <- function(model, ...) {
  rfe(as.MLModel(model), ...)
}
