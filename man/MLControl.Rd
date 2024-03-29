% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MLControl.R
\name{MLControl}
\alias{MLControl}
\alias{controls}
\alias{BootControl}
\alias{BootOptimismControl}
\alias{CVControl}
\alias{CVOptimismControl}
\alias{OOBControl}
\alias{SplitControl}
\alias{TrainControl}
\title{Resampling Controls}
\usage{
BootControl(
  samples = 25,
  weights = TRUE,
  seed = sample(.Machine$integer.max, 1)
)

BootOptimismControl(
  samples = 25,
  weights = TRUE,
  seed = sample(.Machine$integer.max, 1)
)

CVControl(
  folds = 10,
  repeats = 1,
  weights = TRUE,
  seed = sample(.Machine$integer.max, 1)
)

CVOptimismControl(
  folds = 10,
  repeats = 1,
  weights = TRUE,
  seed = sample(.Machine$integer.max, 1)
)

OOBControl(
  samples = 25,
  weights = TRUE,
  seed = sample(.Machine$integer.max, 1)
)

SplitControl(
  prop = 2/3,
  weights = TRUE,
  seed = sample(.Machine$integer.max, 1)
)

TrainControl(weights = TRUE, seed = sample(.Machine$integer.max, 1))
}
\arguments{
\item{samples}{number of bootstrap samples.}

\item{weights}{logical indicating whether to return case weights in resampled
output for the calculation of performance \link{metrics}.}

\item{seed}{integer to set the seed at the start of resampling.}

\item{folds}{number of cross-validation folds (K).}

\item{repeats}{number of repeats of the K-fold partitioning.}

\item{prop}{proportion of cases to include in the training set
(\code{0 < prop < 1}).}
}
\value{
Object that inherits from the \code{MLControl} class.
}
\description{
Structures to define and control sampling methods for estimation of model
predictive performance in the \pkg{MachineShop} package.
}
\details{
\code{BootControl} constructs an \code{MLControl} object for simple bootstrap
resampling in which models are fit with bootstrap resampled training sets and
used to predict the full data set (Efron and Tibshirani 1993).

\code{BootOptimismControl} constructs an \code{MLControl} object for
optimism-corrected bootstrap resampling (Efron and Gong 1983, Harrell et al.
1996).

\code{CVControl} constructs an \code{MLControl} object for repeated K-fold
cross-validation (Kohavi 1995).  In this procedure, the full data set is
repeatedly partitioned into K-folds.  Within a partitioning, prediction is
performed on each of the K folds with models fit on all remaining folds.

\code{CVOptimismControl} constructs an \code{MLControl} object for
optimism-corrected cross-validation resampling (Davison and Hinkley 1997,
eq. 6.48).

\code{OOBControl} constructs an \code{MLControl} object for out-of-bootstrap
resampling in which models are fit with bootstrap resampled training sets and
used to predict the unsampled cases.

\code{SplitControl} constructs an \code{MLControl} object for splitting data
into a separate training and test set (Hastie et al. 2009).

\code{TrainControl} constructs an \code{MLControl} object for training and
performance evaluation to be performed on the same training set (Efron 1986).
}
\examples{
## Bootstrapping with 100 samples
BootControl(samples = 100)

## Optimism-corrected bootstrapping with 100 samples
BootOptimismControl(samples = 100)

## Cross-validation with 5 repeats of 10 folds
CVControl(folds = 10, repeats = 5)

## Optimism-corrected cross-validation with 5 repeats of 10 folds
CVOptimismControl(folds = 10, repeats = 5)

## Out-of-bootstrap validation with 100 samples
OOBControl(samples = 100)

## Split sample validation with 2/3 training and 1/3 testing
SplitControl(prop = 2/3)

## Training set evaluation
TrainControl()

}
\references{
Efron, B., & Tibshirani, R. J. (1993). \emph{An introduction to the
bootstrap}. Chapman & Hall/CRC.

Efron, B., & Gong, G. (1983). A leisurely look at the bootstrap, the
jackknife, and cross-validation. \emph{The American Statistician},
\emph{37}(1), 36-48.

Harrell, F. E., Lee, K. L., & Mark, D. B. (1996). Multivariable prognostic
models: Issues in developing models, evaluating assumptions and adequacy, and
measuring and reducing errors. \emph{Statistics in Medicine}, \emph{15}(4),
361-387.

Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy
estimation and model selection. In \emph{IJCAI'95: Proceedings of the 14th
International Joint Conference on Artificial Intelligence} (vol. 2, pp.
1137-1143). Morgan Kaufmann Publishers Inc.

Davison, A. C., & Hinkley, D. V. (1997). \emph{Bootstrap methods and their
application}. Cambridge University Press.

Hastie, T., Tibshirani, R., & Friedman, J. (2009). \emph{The elements of
statistical learning: data mining, inference, and prediction} (2nd ed.).
Springer.

Efron, B. (1986). How biased is the apparent error rate of a prediction rule?
\emph{Journal of the American Statistical Association}, \emph{81}(394),
461-70.
}
\seealso{
\code{\link{set_monitor}}, \code{\link{set_predict}},
\code{\link{set_strata}},
\code{\link{resample}}, \code{\link{SelectedInput}},
\code{\link{SelectedModel}}, \code{\link{TunedInput}},
\code{\link{TunedModel}}
}
