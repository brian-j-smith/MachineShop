#' Resampling Controls
#'
#' Structures to define and control sampling methods for estimating predictive
#' performance of models in the \pkg{MachineShop} package.
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
#' @param strata_breaks number of quantile bins desired for numeric data
#'   used in stratified resample estimation of model predictive performance.
#' @param strata_nunique number of unique values at or below which numeric
#'   data are stratified as categorical.
#' @param strata_prop minimum proportion of data in each strata.
#' @param strata_size minimum number of values in each strata.
#' @param times,distr,method arguments passed to \code{\link{predict}}.
#' @param seed integer to set the seed at the start of resampling.
#' @param ...  arguments passed to \code{MLControl}.
#'
#' @return \code{MLControl} class object.
#'
#' @seealso \code{\link{resample}}, \code{\link{SelectedInput}},
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
#' Efron B and Tibshirani RJ (1993). An Introduction to the Bootstrap.
#' Monographs on Statistics and Applied Probability 57. Boca Raton, Florida,
#' USA: Chapman & Hall/CRC.
#'
#' @examples
#' ## Bootstrapping with 100 samples
#' BootControl(samples = 100)
#'
BootControl <- function(samples = 25, ...) {
  samples <- check_integer(samples, bounds = c(0, Inf))
  throw(check_assignment(samples))

  new("MLBootControl", MLControl(...), samples = samples)
}


#' @rdname MLControl
#'
#' @details
#' \code{BootOptimismControl} constructs an \code{MLControl} object for
#' optimism-corrected bootstrap resampling (Efron and Gong 1983, Harrell et al. 1996).
#'
#' @references
#' Efron B and Gong G (1983). A leisurely look at the bootstrap, the jackknife,
#' and cross-validation. The American Statistician, 37 (1): 36-48.
#'
#' Harrell FE, Lee KL, and Mark DB (1996). Multivariable prognostic models:
#' issues in developing models, evaluating assumptions and adequacy, and
#' measuring and reducing errors. Statistics in Medicine, 15 (4): 361-387.
#'
#' @examples
#' ## Optimism-corrected bootstrapping with 100 samples
#' BootOptimismControl(samples = 100)
#'
BootOptimismControl <- function(samples = 25, ...) {
  samples <- check_integer(samples, bounds = c(0, Inf))
  throw(check_assignment(samples))

  new("MLBootOptimismControl", MLControl(...), samples = samples)
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
#' Kohavi R (1995). A Study of Cross-Validation and Bootstrap for Accuracy
#' Estimation and Model Selection. In Proceedings of the 14th International
#' Joint Conference on Artificial Intelligence - Volume 2, 1137-43. IJCAI'95.
#' San Francisco, CA, USA: Morgan Kaufmann Publishers Inc.
#'
#' @examples
#' ## Cross-validation with 5 repeats of 10 folds
#' CVControl(folds = 10, repeats = 5)
#'
CVControl <- function(folds = 10, repeats = 1, ...) {
  folds <- check_integer(folds, bounds = c(0, Inf))
  throw(check_assignment(folds))

  repeats <- check_integer(repeats, bounds = c(0, Inf))
  throw(check_assignment(repeats))

  new("MLCVControl", MLControl(...), folds = folds, repeats = repeats)
}


#' @rdname MLControl
#'
#' @details
#' \code{CVOptimismControl} constructs an \code{MLControl} object for
#' optimism-corrected cross-validation resampling (Davison and Hinkley 1997,
#' eq. 6.48).
#'
#' @references
#' Davison AC and Hinkley DV (1997). Bootstrap Methods and Their Application.
#' New York, NY, USA: Cambridge University Press.
#'
#' @examples
#' ## Optimism-corrected cross-validation with 5 repeats of 10 folds
#' CVOptimismControl(folds = 10, repeats = 5)
#'
CVOptimismControl <- function(folds = 10, repeats = 1, ...) {
  folds <- check_integer(folds, bounds = c(0, Inf))
  throw(check_assignment(folds))

  repeats <- check_integer(repeats, bounds = c(0, Inf))
  throw(check_assignment(repeats))

  new("MLCVOptimismControl", MLControl(...), folds = folds, repeats = repeats)
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
OOBControl <- function(samples = 25, ...) {
  samples <- check_integer(samples, bounds = c(0, Inf))
  throw(check_assignment(samples))

  new("MLOOBControl", MLControl(...), samples = samples)
}


#' @rdname MLControl
#'
#' @details
#' \code{SplitControl} constructs an \code{MLControl} object for splitting data
#' into a seperate trianing and test set (Hastie et al. 2009).
#'
#' @references
#' Hastie T, Tibshirani R, and Friedman J (2009). The Elements of Statistical
#' Learning: Data Mining, Inference, and Prediction, Second Edition. Springer
#' Series in Statistics. New York, NY, USA: Springer.
#'
#' @examples
#' ## Split sample validation with 2/3 training and 1/3 testing
#' SplitControl(prop = 2/3)
#'
SplitControl <- function(prop = 2/3, ...) {
  prop <- check_numeric(prop, bounds = c(0, 1))
  throw(check_assignment(prop))

  new("MLSplitControl", MLControl(...), prop = prop)
}


#' @rdname MLControl
#'
#' @details
#' \code{TrainControl} constructs an \code{MLControl} object for training and
#' performance evaluation to be performed on the same training set (Efron 1986).
#'
#' @references
#' Efron B (1986). How biased is the apparent error rate of a prediction rule?
#' Journal of the American Statistical Association, 81 (394): 461-70.
#'
#' @examples
#' ## Training set evaluation
#' TrainControl()
#'
TrainControl <- function(...) {
  new("MLTrainControl", MLControl(...))
}


#' @rdname MLControl
#'
#' @details
#' The base \code{MLControl} constructor initializes a set of control parameters
#' that are common to all resampling methods.
#'
#' Parameters are available to control resampling strata which are constructed
#' from numeric proportions for \code{\link{BinomialVariate}}; original values
#' for \code{character}, \code{factor}, \code{logical}, and \code{ordered};
#' first columns of values for \code{matrix}; original values for
#' \code{numeric}; and numeric times within event statuses for \code{Surv}.
#' Stratification of survival data by event status only can be achieved by
#' setting \code{strata_breaks = 1}.  Numeric values are stratified into
#' quantile bins and categorical values into factor levels.  The number of bins
#' will be the largest integer less than or equal to \code{strata_breaks}
#' satisfying the \code{strata_prop} and \code{strata_size} control argument
#' thresholds.  Categorical levels below the thresholds will be pooled
#' iteratively by reassigning values in the smallest nominal level to the
#' remaining ones at random and by combining the smallest adjacent ordinal
#' levels.  Missing values are replaced with non-missing values sampled at
#' random with replacement.
#'
MLControl <- function(
  strata_breaks = 4, strata_nunique = 5, strata_prop = 0.1, strata_size = 20,
  times = NULL, distr = NULL, method = NULL,
  seed = sample(.Machine$integer.max, 1), ...
) {
  strata_breaks <- check_integer(strata_breaks, bounds = c(0, Inf))
  throw(check_assignment(strata_breaks))

  strata_nunique <- check_integer(strata_nunique, bounds = c(0, Inf))
  throw(check_assignment(strata_nunique))

  strata_prop <- check_numeric(strata_prop, bounds = c(0, 1), include = TRUE)
  throw(check_assignment(strata_prop))

  strata_size <- check_integer(strata_size, bounds = c(0, Inf))
  throw(check_assignment(strata_size))

  new("MLControl", strata_breaks = strata_breaks,
      strata_nunique = strata_nunique, strata_prop = strata_prop,
      strata_size = strata_size, times = times, distr = distr, method = method,
      seed = seed)
}
