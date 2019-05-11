#' Resampling Controls
#' 
#' @description
#' The base \code{MLControl} constructor initializes a set of control parameters
#' that are common to all resampling methods.
#' 
#' @rdname MLControl
#' 
#' @param times,dist,method arguments passed to \code{\link{predict}}.
#' @param seed integer to set the seed at the start of resampling.  This is set
#' to a random integer by default (NULL).
#' @param ...  arguments passed to \code{MLControl}.
#' 
#' @return \code{MLControl} class object.
#' 
#' @seealso \code{\link{resample}}
#' 
MLControl <- function(times = NULL, dist = NULL, method = NULL, seed = NULL,
                      ...) {
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
  new("MLControl", times = times, dist = dist, method = method, seed = seed)
}


#' @description
#' \code{BootControl} constructs an \code{MLControl} object for simple bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the full data set.
#' 
#' @rdname MLControl
#' 
#' @param samples number of bootstrap samples.
#' 
#' @examples
#' ## 100 bootstrap samples
#' BootControl(samples = 100)
#' 
BootControl <- function(samples = 25, ...) {
  new("MLBootControl", MLControl(...), samples = samples)
}


#' @description
#' \code{CVControl} constructs an \code{MLControl} object for repeated K-fold
#' cross-validation.  In this procedure, the full data set is repeatedly
#' partitioned into K-folds.  Within a partitioning, prediction is performed on
#' each of the K folds with models fit on all remaining folds.
#' 
#' @rdname MLControl
#' 
#' @param folds number of cross-validation folds (K).
#' @param repeats number of repeats of the K-fold partitioning.
#' 
#' @examples
#' ## 5 repeats of 10-fold cross-validation
#' CVControl(folds = 10, repeats = 5)
#' 
CVControl <- function(folds = 10, repeats = 1, ...) {
  new("MLCVControl", MLControl(...), folds = folds, repeats = repeats)
}


#' @description
#' \code{OOBControl} constructs an \code{MLControl} object for out-of-bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the unsampled cases.
#' 
#' @rdname MLControl
#' 
#' @examples
#' ## 100 out-of-bootstrap samples
#' OOBControl(samples = 100)
#' 
OOBControl <- function(samples = 25, ...) {
  new("MLOOBControl", MLControl(...), samples = samples)
}


#' @description
#' \code{SplitControl} constructs an \code{MLControl} object for splitting data
#' into a seperate trianing and test set.
#' 
#' @rdname MLControl
#' 
#' @param prop proportion of cases to include in the training set
#' (\code{0 < prop < 1}).
#' 
#' @examples
#' ## Split sample of 2/3 training and 1/3 testing
#' SplitControl(prop = 2/3)
#' 
SplitControl <- function(prop = 2/3, ...) {
  new("MLSplitControl", MLControl(...), prop = prop)
}


#' @description
#' \code{TrainControl} constructs an \code{MLControl} object for training and
#' performance evaluation to be performed on the same training set.
#' 
#' @rdname MLControl
#' 
#' @examples
#' ## Same training and test set
#' TrainControl()
#' 
TrainControl <- function(...) {
  new("MLTrainControl", MLControl(...))
}
