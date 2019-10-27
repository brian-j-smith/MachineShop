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
#' @param seed integer to set the seed at the start of resampling.
#' @param times,dist,method arguments passed to \code{\link{predict}}.
#' @param ...  arguments passed to \code{MLControl}.
#' 
#' @return \code{MLControl} class object.
#' 
#' @seealso \code{\link{resample}}, \code{\link{SelectedModel}},
#' \code{\link{SelectedRecipe}}, \code{\link{TunedModel}},
#' \code{\link{TunedRecipe}}
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
MLControl <- function(times = NULL, dist = NULL, method = NULL,
                      seed = sample(.Machine$integer.max, 1), ...) {
  new("MLControl", times = times, dist = dist, method = method, seed = seed)
}
