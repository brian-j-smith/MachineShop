% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{resample}
\alias{resample}
\alias{resample.formula}
\alias{resample.matrix}
\alias{resample.ModelFrame}
\alias{resample.recipe}
\alias{resample.ModelSpecification}
\alias{resample.MLModel}
\alias{resample.MLModelFunction}
\title{Resample Estimation of Model Performance}
\usage{
resample(...)

\method{resample}{formula}(formula, data, model, ...)

\method{resample}{matrix}(x, y, model, ...)

\method{resample}{ModelFrame}(input, model, ...)

\method{resample}{recipe}(input, model, ...)

\method{resample}{ModelSpecification}(object, control = MachineShop::settings("control"), ...)

\method{resample}{MLModel}(model, ...)

\method{resample}{MLModelFunction}(model, ...)
}
\arguments{
\item{...}{arguments passed from the generic function to its methods, from
the \code{MLModel} and \code{MLModelFunction} methods to first arguments of
others, and from others to the \code{ModelSpecification} method.  The
first argument of each \code{fit} method is positional and, as such, must
be given first in calls to them.}

\item{formula, data}{\link[=formula]{formula} defining the model predictor and
response variables and a \link[=data.frame]{data frame} containing them.}

\item{model}{\link[=models]{model} function, function name, or object; or
another object that can be \link[=as.MLModel]{coerced} to a model.  A model
can be given first followed by any of the variable specifications.}

\item{x, y}{\link{matrix} and object containing predictor and response
variables.}

\item{input}{\link[=inputs]{input} object defining and containing the model
predictor and response variables.}

\item{object}{model \link[=inputs]{input} or
\link[=ModelSpecification]{specification}.}

\item{control}{\link[=controls]{control} function, function name, or object
defining the resampling method to be employed.}
}
\value{
\code{Resample} class object.
}
\description{
Estimation of the predictive performance of a model estimated and evaluated
on training and test samples generated from an observed data set.
}
\details{
Stratified resampling is performed automatically for the \code{formula} and
\code{matrix} methods according to the type of response variable.  In
general, strata are constructed from numeric proportions for
\code{\link{BinomialVariate}}; original values for \code{character},
\code{factor}, \code{logical}, and \code{ordered}; first columns of values
for \code{matrix}; original values for \code{numeric}; and numeric times
within event statuses for \code{Surv}.  Numeric values are stratified into
quantile bins and categorical values into factor levels defined by
\code{\link{MLControl}}.

Resampling stratification variables may be specified manually for
\code{ModelFrames} upon creation with the \code{\link[=ModelFrame]{strata}}
argument in their constructor.  Resampling of this class is unstratified by
default.

Stratification variables may be designated in \code{recipe} specifications
with the \code{\link{role_case}} function.  Resampling will be unstratified
otherwise.
}
\examples{
\donttest{
## Requires prior installation of suggested package gbm to run

## Factor response example

fo <- Species ~ .
control <- CVControl()

gbm_res1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
gbm_res2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
gbm_res3 <- resample(fo, iris, GBMModel(n.trees = 100), control)

summary(gbm_res1)
plot(gbm_res1)

res <- c(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
summary(res)
plot(res)
}

}
\seealso{
\code{\link{c}}, \code{\link{metrics}}, \code{\link{performance}},
\code{\link{plot}}, \code{\link{summary}}
}
