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
  samples = 25, weights = TRUE, seed = sample(.Machine$integer.max, 1)
) {
  samples <- check_integer(samples, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples))

  new_control("BootControl",
    label = "Bootstrap Resampling",
    samples = samples, weights = weights, seed = seed
  ) %>% set_strata
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
  samples = 25, weights = TRUE, seed = sample(.Machine$integer.max, 1)
) {
  new("BootOptimismControl",
    BootControl(samples = samples, weights = weights, seed = seed),
    label = "Optimism-Corrected Bootstrap Resampling"
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
  seed = sample(.Machine$integer.max, 1)
) {
  folds <- check_integer(folds, bounds = c(2, Inf), size = 1)
  throw(check_assignment(folds))

  repeats <- check_integer(repeats, bounds = c(1, Inf), size = 1)
  throw(check_assignment(repeats))

  new_control("CVControl",
    label = "K-Fold Cross-Validation",
    folds = folds, repeats = repeats, weights = weights, seed = seed
  ) %>% set_strata
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
  seed = sample(.Machine$integer.max, 1)
) {
  new("CVOptimismControl",
    CVControl(folds = folds, repeats = repeats, weights = weights, seed = seed),
    label = "Optimism-Corrected K-Fold Cross-Validation"
  )
}


NullControl <- function() {
  new("NullControl", label = "Null Control")
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
  samples = 25, weights = TRUE, seed = sample(.Machine$integer.max, 1)
) {
  samples <- check_integer(samples, bounds = c(1, Inf), size = 1)
  throw(check_assignment(samples))

  new_control("OOBControl",
    label = "Out-of-Bootstrap Resampling",
    samples = samples, weights = weights, seed = seed
  ) %>% set_strata
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
  prop = 2/3, weights = TRUE, seed = sample(.Machine$integer.max, 1)
) {
  prop <- check_numeric(prop, bounds = c(0, 1), include = FALSE, size = 1)
  throw(check_assignment(prop))

  new_control("SplitControl",
    label = "Split Training and Test Samples",
    prop = prop, weights = weights, seed = seed
  ) %>% set_strata
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
  weights = TRUE, seed = sample(.Machine$integer.max, 1)
) {
  new_control("TrainControl",
    label = "Training Resubstitution",
    weights = weights, seed = seed
  )
}


new_control <- function(class, ..., weights, seed) {
  weights <- check_logical(weights, size = 1)
  throw(check_assignment(weights))

  seed <- check_integer(seed, size = 1)
  throw(check_assignment(seed))

  new(class, ..., weights = weights, seed = seed) %>%
    set_monitor %>% set_predict
}
