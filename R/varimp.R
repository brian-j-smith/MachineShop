VarImp <- function(object, ...) {
  UseMethod("VarImp")
}


VarImp.default <- function(object, scale = TRUE, ...) {
  stopifnot(nrow(object) == 0 || is.character(rownames(object)))

  object <- object[rownames(object) != "(Intercept)", , drop = FALSE]
  if (scale) {
    object_shift <- min(object)
    object_scale <- diff(range(object)) / 100
    object <- (object - object_shift) / object_scale
    sort_vals <- rowSums(object)
  } else {
    object_shift = 0
    object_scale = 1
    sort_vals <- object[, 1]
  }
  object <- object[order(sort_vals, decreasing = TRUE), , drop = FALSE]

  new("VarImp", object, shift = object_shift, scale = object_scale)
}


VarImp.matrix <- function(object, ...) {
  VarImp(as.data.frame(object), ...)
}


VarImp.numeric <- function(object, ...) {
  VarImp(cbind(Overall = object), ...)
}


#' Variable Importance
#'
#' Calculate measures of the relative importance of predictors in a model.
#'
#' @param object model \link{fit} result.
#' @param method character string specifying the calculation of variable
#'   importance as permutation-base (\code{"permute"}) or model-specific
#'   (\code{"model"}).  If model-specific importance is specified but not
#'   defined, the permutation-based method will be used instead with its default
#'   values (below).  Permutation-based variable importance is defined as the
#'   relative change in model predictive performances between datasets with and
#'   without permuted values for the associated variable (Fisher et al. 2019).
#' @param scale logical indicating whether importance measures should be scaled
#'   to range from 0 to 100.
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
#'     \item{\code{size = NULL}}{number of observations to sample without
#'       replacement at each round of variable permutations [default: all].
#'       Subsampling of observations will decrease computation time.}
#'     \item{\code{prop = NULL}}{proportion of observations to sample at each
#'       round of permutations [default: all].}
#'     \item{\code{metric = NULL}}{\link[=metrics]{metric} function or function
#'       name with which to calculate performance.  If not specified, the first
#'       applicable default metric from the \link{performance} functions is
#'       used.}
#'     \item{\code{compare = c("-", "/")}}{character specifying the relative
#'       change to compute in comparing model predictive performances between
#'       datasets with and without permuted values.  The choices are difference
#'       (\code{"-"}) and ratio (\code{"/"}).}
#'     \item{\code{stats = MachineShop::settings("stats.VarImp")}}{function,
#'       function name, or vector of these with which to compute summary
#'       statistics on the set of variable importance values from the permuted
#'       datasets.}
#'     \item{\code{na.rm = TRUE}}{logical indicating whether to exclude missing
#'       variable importance values from the calculation of summary statistics.}
#'   }
#'
#' @return \code{VarImp} class object.
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
varimp <- function(object, method = c("permute", "model"), scale = TRUE, ...) {
  stopifnot(is(object, "MLModelFit"))
  model <- as.MLModel(object)
  require_namespaces(model@packages)

  choices <- eval(formals(sys.function())$method)
  if (identical(method, choices)) {
    throw(Warning(
      "The default method has changed from \"model\" to \"permute\"; ",
      "set 'method = \"model\"' to revert to the previous behavior."
    ), times = 3)
  }

  switch(match.arg(method, choices),
    "model" = {
      args <- dep_varimpargs(...)
      vi <- do.call(model@varimp, c(list(unMLModelFit(object)), args))
      if (is.null(vi)) vi <- varimp_permute(object)
    },
    "permute" = {
      args <- eval(substitute(alist(object, ...)))
      vi <- do.call(varimp_permute, args, envir = parent.frame())
    }
  )
  VarImp(vi, scale = scale)
}


dep_varimpargs <- function(metric, ...) {
  args <- list(...)
  if (!missing(metric)) {
    throw(DeprecatedCondition(
      "Argument 'metric' to varimp()", "'type'",
      expired = Sys.Date() >= "2021-12-01"
    ), call = FALSE)
    args$type <- metric
  }
  args
}


varimp_permute <- function(
  object, select = NULL, samples = 1, size = NULL, prop = NULL, metric = NULL,
  compare = c("-", "/"), stats = MachineShop::settings("stats.VarImp"),
  na.rm = TRUE
) {
  x <- as.MLModel(object)@x
  data <- if (is.data.frame(x)) x else as.data.frame(x)
  pred_names <- all.vars(predictors(terms(x, original = TRUE)))
  pred_names <- do.call(subset_names, list(pred_names, substitute(select)),
                        envir = parent.frame())

  samples <- check_integer(samples, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples))

  n <- nrow(data)
  size <- if (is.null(c(size, prop))) {
    n
  } else if (is.null(prop)) {
    size <- check_numeric(size, bounds = c(2, Inf), size = 1)
    throw(check_assignment(size))
    min(size, n)
  } else if (is.null(size)) {
    prop <- check_numeric(prop, bounds = c(0, 1), include = 0:1, size = 1)
    throw(check_assignment(prop))
    max(min(2, n), n * prop)
  } else {
    throw(Error("Arguments 'size' and 'prop' cannot be specified together."))
  }

  compare <- match.arg(compare)
  varimp <- function(x, y) do.call(compare, list(x, y))
  stats <- list_to_function(stats, "stat")

  num_workers <- getDoParWorkers()
  work <- pred_names
  length(work) <- num_workers * ceiling(length(work) / num_workers)
  work_pred_names <- map(na.omit, split(work, seq_len(num_workers)))
  seeds <- rand_int(samples)

  permute_int <- function(n, size = n) {
    inds <- sample.int(n, size)
    half_size <- size / 2
    inds1 <- head(inds, half_size)
    inds2 <- tail(inds, -half_size)
    res <- data.frame(i = c(inds1, inds2), j = c(inds2, inds1))
    res[order(res$i), ]
  }

  pb <- progress_bar$new(
    format = "varimp permute [:bar] :percent | :eta",
    total = samples * length(pred_names),
    show_after = 1
  )
  on.exit(pb$terminate())

  sims <- foreach(
    pred_names = work_pred_names[lengths(work_pred_names) > 0],
    .combine = cbind,
    .export = c("get_MLMetric", "get_perf_metrics"),
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
        pred <- predict(object, newdata, type = "prob")
        if (is.null(metric)) metric <- get_perf_metrics(obs, pred)[[1]]
        metric <- get_MLMetric(metric)
        performance(obs, pred, metrics = metric)[1]
      } else {
        base_perf[1]
      }
      for (name in pred_names) {
        x <- newdata[[name]]
        newdata[[name]] <- data[[name]][inds$j]
        pred <- predict(object, newdata, type = "prob")
        perf[s, name] <- performance(obs, pred, metrics = metric)[1]
        newdata[[name]] <- x
        pb$tick()
      }
    }
    if (metric@maximize) varimp(base_perf, perf) else varimp(perf, base_perf)
  }

  f <- function(x) stats(if (na.rm) na.omit(x) else x)
  res <- do.call(rbind, map(f, as.data.frame(sims)))
  colnames(res) <- paste0("Permute.", colnames(res))
  res
}


varimp_pval <- function(object, ...) {
  UseMethod("varimp_pval")
}


varimp_pval.default <- function(object, ...) {
  varimp_pval(coef(object), diag(vcov(object)), ...)
}


varimp_pval.glm <- function(object, base = exp(1), ...) {
  anova <- drop1(object, test = "Chisq")
  -log(anova[-1, "Pr(>Chi)", drop = FALSE], base = base)
}


varimp_pval.lm <- function(object, base = exp(1), ...) {
  anova <- drop1(object, test = "F")
  -log(anova[-1, "Pr(>F)", drop = FALSE], base = base)
}


varimp_pval.mlm <- function(object, ...) {
  check <- check_equal_weights(object$weights)
  if (is(check, "warning")) {
    throw(LocalWarning("model-specific variable importance not defined for ",
                       class1(object), " with case weights"))
    NULL
  } else {
    object$weights <- NULL
    varimp_pval.default(object, ...)
  }
}


varimp_pval.multinom <- function(object, ...) {
  varimp_pval(t(coef(object)), diag(vcov(object)), ...)
}


varimp_pval.numeric <- function(object, var, base = exp(1), ...) {
  -log(pchisq(object^2 / var, 1, lower.tail = FALSE), base = base)
}
