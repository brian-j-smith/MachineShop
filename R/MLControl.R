#' Resampling Controls
#'
#' Structures to define and control sampling methods for estimation of model
#' predictive performance in the \pkg{MachineShop} package.
#'
#' @name MLControl
#' @rdname MLControl
#' @aliases controls
#'
#' @param folds number of cross-validation folds (K).
#' @param prop proportion of cases to include in the training set
#'   (\code{0 < prop < 1}).
#' @param repeats number of repeats of the K-fold partitioning.
#' @param samples number of bootstrap samples.
#' @param weights logical indicating whether to return case weights in resampled
#'   output for the calculation of performance \link{metrics}.
#' @param seed integer to set the seed at the start of resampling.
#' @param ...  arguments passed to other methods.
#'
#' @return Object that inherits from the \code{MLControl} class.
#'
#' @seealso \code{\link{set_monitor}}, \code{\link{set_predict}},
#' \code{\link{set_strata}},
#' \code{\link{resample}}, \code{\link{SelectedInput}},
#' \code{\link{SelectedModel}}, \code{\link{TunedInput}},
#' \code{\link{TunedModel}}
#'
NULL


#' @rdname MLControl
#'
#' @details
#' \code{BootControl} constructs an \code{MLControl} object for simple bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the full data set (Efron and Tibshirani 1993).
#'
#' @references
#' Efron, B., & Tibshirani, R. J. (1993). \emph{An introduction to the
#' bootstrap}. Chapman & Hall/CRC.
#'
#' @examples
#' ## Bootstrapping with 100 samples
#' BootControl(samples = 100)
#'
BootControl <- function(
  samples = 25, weights = TRUE, seed = sample(.Machine$integer.max, 1), ...
) {
  samples <- check_integer(samples, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples))

  new_control("MLBootControl",
    name = "BootControl", label = "Bootstrap Resampling",
    samples = samples, weights = weights, seed = seed
  ) %>% set_monitor %>% set_strata %>%
    dep_predictargs(...) %>% dep_strataargs(...)
}


#' @rdname MLControl
#'
#' @details
#' \code{BootOptimismControl} constructs an \code{MLControl} object for
#' optimism-corrected bootstrap resampling (Efron and Gong 1983, Harrell et al.
#' 1996).
#'
#' @references
#' Efron, B., & Gong, G. (1983). A leisurely look at the bootstrap, the
#' jackknife, and cross-validation. \emph{The American Statistician},
#' \emph{37}(1), 36-48.
#'
#' Harrell, F. E., Lee, K. L., & Mark, D. B. (1996). Multivariable prognostic
#' models: Issues in developing models, evaluating assumptions and adequacy, and
#' measuring and reducing errors. \emph{Statistics in Medicine}, \emph{15}(4),
#' 361-387.
#'
#' @examples
#' ## Optimism-corrected bootstrapping with 100 samples
#' BootOptimismControl(samples = 100)
#'
BootOptimismControl <- function(
  samples = 25, weights = TRUE, seed = sample(.Machine$integer.max, 1), ...
) {
  new_control("MLBootOptimismControl",
    BootControl(samples = samples, ...),
    name = "BootOptimismControl",
    label = "Optimism-Corrected Bootstrap Resampling",
    weights = weights, seed = seed
  )
}


#' @rdname MLControl
#'
#' @details
#' \code{CVControl} constructs an \code{MLControl} object for repeated K-fold
#' cross-validation (Kohavi 1995).  In this procedure, the full data set is
#' repeatedly partitioned into K-folds.  Within a partitioning, prediction is
#' performed on each of the K folds with models fit on all remaining folds.
#'
#' @references
#' Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy
#' estimation and model selection. In \emph{IJCAI'95: Proceedings of the 14th
#' International Joint Conference on Artificial Intelligence} (vol. 2, pp.
#' 1137-1143). Morgan Kaufmann Publishers Inc.
#'
#' @examples
#' ## Cross-validation with 5 repeats of 10 folds
#' CVControl(folds = 10, repeats = 5)
#'
CVControl <- function(
  folds = 10, repeats = 1, weights = TRUE,
  seed = sample(.Machine$integer.max, 1), ...
) {
  folds <- check_integer(folds, bounds = c(2, Inf), size = 1)
  throw(check_assignment(folds))

  repeats <- check_integer(repeats, bounds = c(1, Inf), size = 1)
  throw(check_assignment(repeats))

  new_control("MLCVControl",
    name = "CVControl", label = "K-Fold Cross-Validation",
    folds = folds, repeats = repeats, weights = weights, seed = seed
  ) %>% set_monitor %>% set_strata %>%
    dep_predictargs(...) %>% dep_strataargs(...)
}


#' @rdname MLControl
#'
#' @details
#' \code{CVOptimismControl} constructs an \code{MLControl} object for
#' optimism-corrected cross-validation resampling (Davison and Hinkley 1997,
#' eq. 6.48).
#'
#' @references
#' Davison, A. C., & Hinkley, D. V. (1997). \emph{Bootstrap methods and their
#' application}. Cambridge University Press.
#'
#' @examples
#' ## Optimism-corrected cross-validation with 5 repeats of 10 folds
#' CVOptimismControl(folds = 10, repeats = 5)
#'
CVOptimismControl <- function(
  folds = 10, repeats = 1, weights = TRUE,
  seed = sample(.Machine$integer.max, 1), ...
) {
  new("MLCVOptimismControl",
    CVControl(folds = folds, repeats = repeats, ...),
    name = "CVOptimismControl",
    label = "Optimism-Corrected K-Fold Cross-Validation",
    weights = weights, seed = seed
  )
}


#' @rdname MLControl
#'
#' @details
#' \code{OOBControl} constructs an \code{MLControl} object for out-of-bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the unsampled cases.
#'
#' @examples
#' ## Out-of-bootstrap validation with 100 samples
#' OOBControl(samples = 100)
#'
OOBControl <- function(
  samples = 25, weights = TRUE, seed = sample(.Machine$integer.max, 1), ...
) {
  samples <- check_integer(samples, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples))

  new_control("MLOOBControl",
    name = "OOBControl", label = "Out-of-Bootstrap Resampling",
    samples = samples, weights = weights, seed = seed
  ) %>% set_monitor %>% set_strata %>%
    dep_predictargs(...) %>% dep_strataargs(...)
}


#' @rdname MLControl
#'
#' @details
#' \code{SplitControl} constructs an \code{MLControl} object for splitting data
#' into a separate training and test set (Hastie et al. 2009).
#'
#' @references
#' Hastie, T., Tibshirani, R., & Friedman, J. (2009). \emph{The elements of
#' statistical learning: data mining, inference, and prediction} (2nd ed.).
#' Springer.
#'
#' @examples
#' ## Split sample validation with 2/3 training and 1/3 testing
#' SplitControl(prop = 2/3)
#'
SplitControl <- function(
  prop = 2/3, weights = TRUE, seed = sample(.Machine$integer.max, 1), ...
) {
  prop <- check_numeric(prop, bounds = c(0, 1), include = FALSE, size = 1)
  throw(check_assignment(prop))

  new_control("MLSplitControl",
    name = "SplitControl", label = "Split Training and Test Samples",
    prop = prop, weights = weights, seed = seed
  ) %>% set_strata %>% dep_predictargs(...) %>% dep_strataargs(...)
}


#' @rdname MLControl
#'
#' @details
#' \code{TrainControl} constructs an \code{MLControl} object for training and
#' performance evaluation to be performed on the same training set (Efron 1986).
#'
#' @references
#' Efron, B. (1986). How biased is the apparent error rate of a prediction rule?
#' \emph{Journal of the American Statistical Association}, \emph{81}(394),
#' 461-70.
#'
#' @examples
#' ## Training set evaluation
#' TrainControl()
#'
TrainControl <- function(
  weights = TRUE, seed = sample(.Machine$integer.max, 1), ...
) {
  new_control("MLTrainControl",
    name = "TrainControl", label = "Training Resubstitution",
    weights = weights, seed = seed
  ) %>% dep_predictargs(...) %>% dep_strataargs(...)
}


new_control <- function(class, ..., weights, seed) {
  weights <- check_logical(weights, size = 1)
  throw(check_assignment(weights))

  seed <- check_integer(seed, size = 1)
  throw(check_assignment(seed))

  new(class, ..., weights = weights, seed = seed) %>% set_predict
}


dep_predictargs <- function(x, times, distr, method, ...) {
  args <- list()
  if (!missing(times)) args$times <- times
  if (!missing(distr)) args$distr <- distr
  if (!missing(method)) args$method <- method
  if (length(args)) {
    throw(
      DeprecatedCondition(
        "Argument 'times', 'distr', and 'method' to MLControl()",
        "'set_predict()'", expired = Sys.Date() >= "2021-10-01"
      ),
      call = FALSE
    )
    x <- do.call(set_predict, c(list(x), args))
  }
  x
}


dep_strataargs <- function(
  x, strata_breaks, strata_nunique, strata_prop, strata_size, ...
) {
  args <- list()
  if (!missing(strata_breaks)) args$breaks <- strata_breaks
  if (!missing(strata_nunique)) args$nunique <- strata_nunique
  if (!missing(strata_prop)) args$prop <- strata_prop
  if (!missing(strata_size)) args$size <- strata_size
  if (length(args)) {
    old <- paste("Argument 'strata_breaks', 'strata_nunique', 'strata_prop',",
                 "and 'strata_size' to MLControl()")
    throw(
      DeprecatedCondition(
        old, "'set_strata()'", expired = Sys.Date() >= "2021-10-01"
      ),
      call = FALSE
    )
    x <- do.call(set_strata, c(list(x), args))
  }
  x
}


#' Resampling Monitoring Control
#'
#' Set parameters that control the monitoring of resample estimation of model
#' performance.
#'
#' @param control \link[=controls]{control} object.
#' @param progress logical indicating whether to display a progress bar during
#'   resampling if a computing cluster is not registered or is registered with
#'   the \pkg{doSNOW} package.
#' @param verbose logical indicating whether to enable verbose messages which
#'   may be useful for trouble shooting.
#'
#' @return Argument \code{control} updated with the supplied parameters.
#'
#' @seealso \code{\link{set_predict}}, \code{\link{set_strata}},
#' \code{\link{resample}}, \code{\link{SelectedInput}},
#' \code{\link{SelectedModel}}, \code{\link{TunedInput}},
#' \code{\link{TunedModel}}
#'
#' @examples
#' CVControl() %>% set_monitor(verbose = TRUE)
#'
set_monitor <- function(control, progress = TRUE, verbose = FALSE) {
  stopifnot(is(control, "MLControl"))

  progress <- check_logical(progress, size = 1)
  throw(check_assignment(progress))

  verbose <- check_logical(verbose, size = 1)
  throw(check_assignment(verbose))

  control@monitor <- list(progress = progress, verbose = verbose)
  control
}


#' Resampling Prediction Control
#'
#' Set parameters that control prediction during resample estimation of model
#' performance.
#'
#' @param control \link[=controls]{control} object.
#' @param times,distr,method arguments passed to \code{\link{predict}}.
#'
#' @return Argument \code{control} updated with the supplied parameters.
#'
#' @seealso \code{\link{set_monitor}}, \code{\link{set_strata}},
#' \code{\link{resample}}, \code{\link{SelectedInput}},
#' \code{\link{SelectedModel}}, \code{\link{TunedInput}},
#' \code{\link{TunedModel}}
#'
#' @examples
#' CVControl() %>% set_predict(times = 1:3)
#'
set_predict <- function(control, times = NULL, distr = NULL, method = NULL) {
  stopifnot(is(control, "MLControl"))

  if (!is.null(times)) {
    times <- check_numeric(times, bounds = c(0, Inf), include = 0, size = NA)
  }
  throw(check_assignment(times))

  if (!is.null(distr)) distr <- check_character(distr, size = 1)
  throw(check_assignment(distr))

  if (!is.null(method)) method <- check_character(method, size = 1)
  throw(check_assignment(method))

  control@predict <- list(times = times, distr = distr, method = method)
  control
}


#' Resampling Stratification Control
#'
#' Set parameters that control the construction of strata during resample
#' estimation of model performance.
#'
#' @param control \link[=controls]{control} object.
#' @param breaks number of quantile bins desired for stratification of numeric
#'   data during resampling.
#' @param nunique number of unique values at or below which numeric data are
#'   stratified as categorical.
#' @param prop minimum proportion of data in each strata.
#' @param size minimum number of values in each strata.
#'
#' @details
#' The arguments control resampling strata which are constructed from numeric
#' proportions for \code{\link{BinomialVariate}}; original values for
#' \code{character}, \code{factor}, \code{logical}, \code{numeric}, and
#' \code{ordered}; first columns of values for \code{matrix}; and numeric times
#' within event statuses for \code{Surv}.  Stratification of survival data by
#' event status only can be achieved by setting \code{breaks = 1}.  Numeric
#' values are stratified into quantile bins and categorical values into factor
#' levels.  The number of bins will be the largest integer less than or equal to
#' \code{breaks} satisfying the \code{prop} and \code{size} control argument
#' thresholds.  Categorical levels below the thresholds will be pooled
#' iteratively by reassigning values in the smallest nominal level to the
#' remaining ones at random and by combining the smallest adjacent ordinal
#' levels.  Missing values are replaced with non-missing values sampled at
#' random with replacement.
#'
#' @return Argument \code{control} updated with the supplied parameters.
#'
#' @seealso \code{\link{set_monitor}}, \code{\link{set_predict}},
#' \code{\link{resample}}, \code{\link{SelectedInput}},
#' \code{\link{SelectedModel}}, \code{\link{TunedInput}},
#' \code{\link{TunedModel}}
#'
#' @examples
#' CVControl() %>% set_strata(breaks = 3)
#'
set_strata <- function(control, breaks = 4, nunique = 5, prop = 0.1, size = 20) {
  stopifnot(is(control, "MLControl"))

  breaks <- check_integer(breaks, bounds = c(1, Inf), size = 1)
  throw(check_assignment(breaks))

  nunique <- check_integer(nunique, bounds = c(1, Inf), size = 1)
  throw(check_assignment(nunique))

  prop <- check_numeric(prop, bounds = c(0, 1), size = 1)
  throw(check_assignment(prop))

  size <- check_integer(size, bounds = c(1, Inf), size = 1)
  throw(check_assignment(size))

  control@strata <- list(breaks = breaks, nunique = nunique, prop = prop, size = size)
  control
}
