VariableImportance <- function(object, ...) {
  UseMethod("VariableImportance")
}


VariableImportance.data.frame <- function(
  object, scale = TRUE, sort = "decreasing", ...
) {
  stopifnot(nrow(object) == 0 || is.character(rownames(object)))

  object <- object[rownames(object) != "(Intercept)", , drop = FALSE]
  scale <- rep_len(scale, ncol(object))
  if (any(scale)) {
    scale_num <- max(object[scale], 0) / 100
    object[scale] <- object[scale] / scale_num
    if (!all(scale)) scale_num <- ifelse(scale, scale_num, NA_real_)
    sort_vals <- if (scale[1]) rowSums(object[scale]) else object[[1]]
  } else {
    scale_num <- NA_real_
    sort_vals <- object[[1]]
  }
  if (sort != "asis") {
    sort_order <- order(sort_vals, decreasing = sort == "decreasing")
    object <- object[sort_order, , drop = FALSE]
  }

  new("VariableImportance", object, scale = scale_num)
}


VariableImportance.matrix <- function(object, ...) {
  VariableImportance(as.data.frame(object), ...)
}


VariableImportance.numeric <- function(object, ...) {
  VariableImportance(cbind(Overall = object), ...)
}


VariableImportance.NULL <- function(object, ...) {
  throw(Error("No variables for which to compute importance."))
}


#' Variable Importance
#'
#' Calculate measures of relative importance for model predictor variables.
#'
#' @rdname varimp
#'
#' @param object model \link{fit} result.
#' @param method character string specifying the calculation of variable
#'   importance as permutation-base (\code{"permute"}) or model-specific
#'   (\code{"model"}).  If model-specific importance is specified but not
#'   defined, the permutation-based method will be used instead with its default
#'   values (below).  Permutation-based variable importance is defined as the
#'   relative change in model predictive performances between datasets with and
#'   without permuted values for the associated variable (Fisher et al. 2019).
#' @param scale logical value or vector indicating whether importance values are
#'   scaled to a maximum of 100.
#' @param sort character string specifying the sort order of importance values
#'   to be \code{"decreasing"}, \code{"increasing"}, or as predictors appear in
#'   the model formula (\code{"asis"}).
#' @param ... arguments passed to model-specific or permutation-based variable
#'   importance functions.  These include the following arguments and default
#'   values for \code{method = "permute"}.
#'   \describe{
#'     \item{\code{select = NULL}}{expression indicating predictor variables for
#'       which to compute variable importance (see \code{\link[base]{subset}}
#'       for syntax) [default: all].}
#'     \item{\code{samples = 1}}{number of times to permute the values of each
#'       variable.  Larger numbers of samples decrease variability in the
#'       estimates at the expense of increased computation time.}
#'     \item{\code{prop = numeric()}}{proportion of observations to sample
#'       without replacement at each round of variable permutations [default:
#'       all].  Subsampling of observations can decrease computation time.}
#'     \item{\code{size = integer()}}{number of observations to sample at each
#'       round of permutations [default: all].}
#'     \item{\code{times = numeric()}}{numeric vector of follow-up times at
#'       which to predict survival probabilities or \code{NULL} for predicted
#'       survival means.}
#'     \item{\code{metric = NULL}}{\link[=metrics]{metric} function or function
#'       name with which to calculate performance.  If not specified, the first
#'       applicable default metric from the \link{performance} functions is
#'       used.}
#'     \item{\code{compare = c("-", "/")}}{character specifying the relative
#'       change to compute in comparing model predictive performances between
#'       datasets with and without permuted values.  The choices are difference
#'       (\code{"-"}) and ratio (\code{"/"}).}
#'     \item{\code{stats = MachineShop::settings("stat.TrainingParams")}}{
#'       function, function name, or vector of these with which to compute
#'       summary statistics on the set of variable importance values from the
#'       permuted datasets.}
#'     \item{\code{na.rm = TRUE}}{logical indicating whether to exclude missing
#'       variable importance values from the calculation of summary statistics.}
#'     \item{\code{progress = TRUE}}{logical indicating whether to display
#'       iterative progress during computation.}
#'   }
#'
#' @details
#' The \code{varimp} function supports calculation of variable importance with
#' the permutation-based method of Fisher et al. (2019) or with model-based
#' methods where defined.  Permutation-based importance is the default and has
#' the advantages of being available for any model, any performance metric
#' defined for the associated response variable type, and any predictor variable
#' in the original training dataset.  Conversely, model-specific importance is
#' not defined for some models and will fall back to the permutation method in
#' such cases; is generally limited to metrics implemented in the source
#' packages of models; and may be computed on derived, rather than original,
#' predictor variables.  These disadvantages can make comparisons of
#' model-specific importance across different classes of models infeasible.  A
#' downside of the permutation-based approach is increased computation time.  To
#' counter this, the permutation algorithm can be run in parallel simply by
#' loading a parallel backend for the \pkg{foreach} package \code{\%dopar\%}
#' function, such as \pkg{doParallel} or \pkg{doSNOW}.
#'
#' Permutation variable importance is interpreted as the contribution of a
#' predictor variable to the predictive performance of a model as measured by
#' the performance metric used in the calculation.  Importance of a predictor is
#' conditional on and, with the default scaling, relative to the values of all
#' other predictors in the analysis.
#'
#' @return \code{VariableImportance} class object.
#'
#' @references
#' Fisher, A., Rudin, C., & Dominici, F. (2019). All models are wrong, but many
#' are useful: Learning a variable's importance by studying an entire class of
#' prediction models simultaneously. \emph{Journal of Machine Learning
#' Research}, \emph{20}, 1-81.
#'
#' @seealso \code{\link{plot}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Survival response example
#' library(survival)
#'
#' gbm_fit <- fit(Surv(time, status) ~ ., data = veteran, model = GBMModel)
#' (vi <- varimp(gbm_fit))
#' plot(vi)
#' }
#'
varimp <- function(
  object, method = c("permute", "model"), scale = TRUE,
  sort = c("decreasing", "increasing", "asis"), ...
) {
  stopifnot(is(object, "MLModelFit"))
  object <- update(object)
  model <- as.MLModel(object)
  throw(check_packages(model@packages))

  sort <- match.arg(sort)

  switch(match.arg(method),
    "model" = {
      .MachineShop <- attr(object, ".MachineShop")
      vi <- model@varimp(unMLModelFit(object), .MachineShop = .MachineShop, ...)
      if (is.null(vi)) {
        throw(Warning(
          "Model-specific variable importance is not available; ",
          "computing permutation-based importance instead."
        ))
        vi <- varimp_permute(object)
      }
    },
    "permute" = {
      args <- eval(substitute(alist(object, ...)))
      vi <- do.call(varimp_permute, args, envir = parent.frame())
    }
  )
  VariableImportance(vi, scale = scale, sort = sort)
}


varimp_permute <- function(
  object, select = NULL, samples = 1, prop = numeric(), size = integer(),
  times = numeric(), metric = NULL, compare = c("-", "/"),
  stats = MachineShop::settings("stat.TrainingParams"), na.rm = TRUE,
  progress = TRUE
) {
  input <- as.MLInput(object)
  data <- if (is.data.frame(input)) input else as.data.frame(input)
  pred_names <- all.vars(predictors(terms(input, original = TRUE)))
  pred_names <- do.call(subset_names, list(pred_names, substitute(select)),
                        envir = parent.frame())

  samples <- check_integer(samples, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples))

  n <- nrow(data)
  size <- if (is_empty(c(prop, size))) {
    n
  } else if (is_empty(size)) {
    prop <- check_numeric(prop, bounds = c(0, 1), include = 0:1, size = 1)
    throw(check_assignment(prop))
    min(max(prop * n, 2), n)
  } else if (is_empty(prop)) {
    size <- check_numeric(size, bounds = c(2, Inf), size = 1)
    throw(check_assignment(size))
    min(size, n)
  } else {
    throw(Error("Arguments `prop` and `size` cannot be specified together."))
  }

  if (!is.null(metric)) {
    metric <- check_metric(metric, convert = TRUE)
    throw(check_assignment(metric))
  }
  compare <- match.arg(compare)
  stats <- check_stats(stats, convert = TRUE)
  throw(check_assignment(stats))

  varimp <- function(x, baseline, maximize = FALSE) {
    compare_args <- if (maximize) list(baseline, x) else list(x, baseline)
    do.call(rbind,
      apply(do.call(compare, compare_args), 2, function(col) {
        stats(if (na.rm) na.omit(col) else col)
      }, simplify = FALSE)
    )
  }

  progress <- if (throw(check_logical(progress))) {
    pb <- progress_bar$new(
      format = "varimp permute [:bar] :percent | :eta",
      total = samples * length(pred_names),
      show_after = 1
    )
    on.exit(pb$terminate())
    function() pb$tick()
  } else {
    function() NULL
  }

  work <- pred_names
  num_workers <- getDoParWorkers()
  num_tasks <- ceiling(length(work) / num_workers)
  length(work) <- num_workers * num_tasks
  jobs <- map(na.omit, split(work, rep(1:num_workers, each = num_tasks)))
  seeds <- rand_int(samples)

  foreach(
    pred_names = jobs[lengths(jobs) > 0],
    .combine = rbind,
    .inorder = TRUE,
    .export = c("as.MLMetric", "get_perf_metrics", "map", "permute_int"),
    .packages = "MachineShop"
  ) %dopar% {
    subset <- size < n
    base_perf <- rep(NA_real_, samples)
    perf <- matrix(NA_real_, samples, length(pred_names))
    colnames(perf) <- pred_names
    for (s in seq_len(samples)) {
      set.seed(seeds[s])
      inds <- permute_int(n, size)
      base_perf[s] <- if (s == 1 || subset) {
        newdata <- if (subset) data[inds$i, ] else data
        obs <- response(object, newdata)
        pred <- predict(object, newdata, times = times, type = "default")
        if (is.null(metric)) {
          metric <- as.MLMetric(get_perf_metrics(obs, pred)[[1]])
        }
        metric(obs, pred)[1]
      } else {
        base_perf[1]
      }
      for (name in pred_names) {
        x <- newdata[[name]]
        newdata[[name]] <- data[[name]][inds$j]
        pred <- predict(object, newdata, times = times, type = "default")
        perf[s, name] <- metric(obs, pred)[1]
        newdata[[name]] <- x
        progress()
      }
    }
    res <- varimp(perf, base_perf, metric@maximize)
    colnames(res) <- paste0("Permute.", colnames(res), ".", metric@name)
    res
  }
}


varimp_pval <- function(object, ...) {
  UseMethod("varimp_pval")
}


varimp_pval.default <- function(object, test = "Chisq", base = exp(1), ...) {
  res <- drop1(object, test = test)[-1, , drop = FALSE]
  structure(-log(res[[ncol(res)]], base = base), names = rownames(res))
}


varimp_pval.mlm <- function(object, ...) {
  check <- check_equal_weights(object$weights)
  if (is(check, "warning")) {
    throw(LocalWarning(
      "Model-specific variable importance not defined for ", class1(object),
      " with case weights."
    ))
    NULL
  } else {
    object$weights <- NULL
    varimp_pval(coef(object), diag(vcov(object)), ...)
  }
}


varimp_pval.multinom <- function(object, ...) {
  varimp_pval(t(coef(object)), diag(vcov(object)), ...)
}


varimp_pval.numeric <- function(object, var, base = exp(1), ...) {
  -log(pchisq(object^2 / var, 1, lower.tail = FALSE), base = base)
}
