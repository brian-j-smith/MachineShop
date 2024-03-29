% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance_curve.R
\name{performance_curve}
\alias{performance_curve}
\alias{curves}
\alias{performance_curve.default}
\alias{performance_curve.Resample}
\title{Model Performance Curves}
\usage{
performance_curve(x, ...)

\method{performance_curve}{default}(
  x,
  y,
  weights = NULL,
  metrics = c(MachineShop::tpr, MachineShop::fpr),
  na.rm = TRUE,
  ...
)

\method{performance_curve}{Resample}(
  x,
  metrics = c(MachineShop::tpr, MachineShop::fpr),
  na.rm = TRUE,
  ...
)
}
\arguments{
\item{x}{\link[=response]{observed responses} or \link{resample} result
containing observed and predicted responses.}

\item{...}{arguments passed to other methods.}

\item{y}{\link[=predict]{predicted responses} if not contained in \code{x}.}

\item{weights}{numeric vector of non-negative
\link[=case_weights]{case weights} for the observed \code{x} responses
[default: equal weights].}

\item{metrics}{list of two performance \link{metrics} for the analysis
[default: ROC metrics].  Precision recall curves can be obtained with
\code{c(precision, recall)}.}

\item{na.rm}{logical indicating whether to remove observed or predicted
responses that are \code{NA} when calculating metrics.}
}
\value{
\code{PerformanceCurve} class object that inherits from
\code{data.frame}.
}
\description{
Calculate curves for the analysis of tradeoffs between metrics for assessing
performance in classifying binary outcomes over the range of possible
cutoff probabilities.  Available curves include receiver operating
characteristic (ROC) and precision recall.
}
\examples{
\donttest{
## Requires prior installation of suggested package gbm to run

data(Pima.tr, package = "MASS")

res <- resample(type ~ ., data = Pima.tr, model = GBMModel)

## ROC curve
roc <- performance_curve(res)
plot(roc)
auc(roc)
}

}
\seealso{
\code{\link{auc}}, \code{\link{c}}, \code{\link{plot}},
\code{\link{summary}}
}
