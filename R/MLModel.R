#' MLModel Class Constructor
#' 
#' Create a model for use with the \pkg{MachineShop} package.
#' 
#' @param name character string name for the instantiated \code{MLModel} object;
#' same name as the object to which the model is assigned.
#' @param label descriptive label for the model.
#' @param packages character vector of packages whose namespaces are required by
#' the model.
#' @param types character vector of response variable types to which the model
#' can be fit.  Supported types are \code{"binary"}, \code{"factor"},
#' \code{"matrix"}, \code{"numeric"}, \code{"ordered"}, and \code{"Surv"}.
#' @param params list of user-specified model parameters to be passed to the
#' \code{fit} function.
#' @param grid tuning grid function whose first agument \code{x} is a
#' \code{\link{ModelFrame}} of the model fit data and formula, followed by a
#' \code{length} to use in generating sequences of parameter values, a number of
#' grid points to sample at \code{random}, and an ellipsis (\code{...}).
#' @param design character string indicating whether the type of design matrix
#' used to fit the model is a \code{"\link{model.matrix}"}, a data.frame
#' of the original predictor variable \code{"terms"}, or unknown (default).
#' @param fit model fitting function whose arguments are a \code{formula}, a
#' \code{\link{ModelFrame}} named \code{data}, case \code{weights}, and an
#' ellipsis.
#' @param predict model prediction function whose arguments are the
#' \code{object} returned by \code{fit}, a \code{\link{ModelFrame}} named
#' \code{newdata} of predictor variables, optional vector of \code{times} at
#' which to predict survival, and an ellipsis.
#' @param varimp variable importance function whose arguments are the
#' \code{object} returned by \code{fit}, optional arguments passed from calls
#' to \code{\link{varimp}}, and an ellipsis.
#' @param ... arguments passed from other methods.
#' 
#' @details
#' If supplied, the \code{grid} function should return a list whose elements are
#' named after and contain values of parameters to include in a tuning grid to
#' be constructed automatically by the package.
#' 
#' Values returned by the \code{predict} functions should be formatted according
#' to the response variable types below.
#' \describe{
#' \item{factor}{a vector or column matrix of probabilities for the second level
#' of binary factors or a matrix whose columns contain the probabilities for
#' factors with more than two levels.}
#' \item{matrix}{a matrix of predicted responses.}
#' \item{numeric}{a vector or column matrix of predicted responses.}
#' \item{Surv}{a matrix whose columns contain survival probabilities at
#' \code{times} if supplied or a vector of predicted survival means otherwise.}
#' }
#' 
#' The \code{varimp} function should return a vector of importance values named
#' after the predictor variables or a matrix or data frame whose rows are named
#' after the predictors.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link{modelinfo}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' ## Logistic regression model
#' LogisticModel <- MLModel(
#'   name = "LogisticModel",
#'   types = "binary",
#'   fit = function(formula, data, weights, ...) {
#'     glm(formula, data = data, weights = weights, family = binomial, ...)
#'   },
#'   predict = function(object, newdata, ...) {
#'     predict(object, newdata = newdata, type = "response")
#'   },
#'   varimp = function(object, ...) {
#'     pchisq(coef(object)^2 / diag(vcov(object)), 1)
#'   }
#' )
#' 
#' library(MASS)
#' res <- resample(type ~ ., data = Pima.tr, model = LogisticModel)
#' summary(res)
#' 
MLModel <- function(name = "MLModel", label = name, packages = character(),
                    types = character(), params = list(),
                    grid = function(x, length, random, ...) NULL,
                    design = c(NA, "model.matrix", "terms"),
                    fit = function(formula, data, weights, ...)
                      stop("no fit function"),
                    predict = function(object, newdata, times, ...)
                      stop("no predict function"),
                    varimp = function(object, ...) NULL, ...) {
  
  stopifnot(types %in% c("binary", "factor", "matrix", "numeric", "ordered",
                         "Surv"))
  
  new("MLModel",
      name = name,
      label = label,
      packages = packages,
      types = types,
      params = params,
      grid = grid,
      design = match.arg(design),
      fit = fit,
      fitbits = MLFitBits(packages = packages,
                          predict = predict,
                          varimp = varimp))
}


MLFitBits <- function(...) {
  new("MLFitBits", ...)
}
