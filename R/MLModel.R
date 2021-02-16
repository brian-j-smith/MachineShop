#' MLModel Class Constructor
#'
#' Create a model for use with the \pkg{MachineShop} package.
#'
#' @param name character name of the object to which the model is assigned.
#' @param label optional character descriptor for the model.
#' @param packages character vector of packages required to use the model.
#' @param response_types character vector of response variable types to which
#'   the model can be fit.  Supported types are \code{"binary"}, =
#'   \code{"BinomialVariate"}, \code{"DiscreteVariate"}, \code{"factor"},
#'   \code{"matrix"}, \code{"NegBinomialVariate"}, \code{"numeric"},
#'   \code{"ordered"}, \code{"PoissonVariate"}, and \code{"Surv"}.
#' @param predictor_encoding character string indicating whether the model is
#'   fit with predictor variables encoded as a \code{"\link{model.matrix}"}, a
#'   data.frame containing the originally specified model \code{"terms"}, or
#'   unspecified (default).
#' @param params list of user-specified model parameters to be passed to the
#'   \code{fit} function.
#' @param gridinfo tibble of information for construction of tuning grids
#'   consisting of a character column \code{param} with the names of parameters
#'   in the grid, a list column \code{values} with functions to generate grid
#'   points for the corresponding parameters, and an optional logical column
#'   \code{regular} indicating which parameters to include by default in regular
#'   grids.  Values functions may optionally include arguments \code{n} and
#'   \code{data} for the number of grid points to generate and a
#'   \code{\link{ModelFrame}} of the model fit data and formula, respectively;
#'   and must include an ellipsis (\code{...}).
#' @param fit model fitting function whose arguments are a \code{formula}, a
#'   \code{\link{ModelFrame}} named \code{data}, case \code{weights}, and an
#'   ellipsis.
#' @param predict model prediction function whose arguments are the
#'   \code{object} returned by \code{fit}, a \code{\link{ModelFrame}} named
#'   \code{newdata} of predictor variables, optional vector of \code{times} at
#'   which to predict survival, and an ellipsis.
#' @param varimp variable importance function whose arguments are the
#'   \code{object} returned by \code{fit}, optional arguments passed from calls
#'   to \code{\link{varimp}}, and an ellipsis.
#' @param ... arguments passed from other methods.
#'
#' @details
#' If supplied, the \code{grid} function should return a list whose elements are
#' named after and contain values of parameters to include in a tuning grid to
#' be constructed automatically by the package.
#'
#' Argument \code{data} in the \code{fit} function may be converted to a data
#' frame with the \code{as.data.frame} function as needed.  The function should
#' return the object resulting from the model fit.
#'
#' Values returned by the \code{predict} functions should be formatted according
#' to the response variable types below.
#' \describe{
#'   \item{factor}{vector or column matrix of probabilities for the second level
#'     of binary factors or a matrix whose columns contain the probabilities for
#'     factors with more than two levels.}
#'   \item{matrix}{matrix of predicted responses.}
#'   \item{numeric}{vector or column matrix of predicted responses.}
#'   \item{Surv}{matrix whose columns contain survival probabilities at
#'     \code{times} if supplied or a vector of predicted survival means
#'     otherwise.}
#' }
#'
#' The \code{varimp} function should return a vector of importance values named
#' after the predictor variables or a matrix or data frame whose rows are named
#' after the predictors.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link{models}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' ## Logistic regression model
#' LogisticModel <- MLModel(
#'   name = "LogisticModel",
#'   response_types = "binary",
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
#' data(Pima.tr, package = "MASS")
#' res <- resample(type ~ ., data = Pima.tr, model = LogisticModel)
#' summary(res)
#'
MLModel <- function(name = "MLModel", label = name, packages = character(),
                    response_types = character(),
                    predictor_encoding = c(NA, "model.matrix", "terms"),
                    params = list(),
                    gridinfo = tibble::tibble(
                      param = character(), values = list(), regular = logical()
                    ),
                    fit = function(formula, data, weights, ...)
                      stop("no fit function"),
                    predict = function(object, newdata, times, ...)
                      stop("no predict function"),
                    varimp = function(object, ...) NULL, ...) {

  stopifnot(response_types %in% settings("response_types"))

  if (is.function(gridinfo)) {
    depwarn("'grid' argument to MLModel() is deprecated",
            "use 'gridinfo' argument with a tibble instead",
            expired = Sys.Date() >= "2021-04-01")
    gridinfo <- new_gridinfo()
  }

  new("MLModel",
      name = name,
      label = label,
      packages = packages,
      response_types = response_types,
      predictor_encoding = match.arg(predictor_encoding),
      params = params,
      gridinfo = gridinfo,
      fit = fit,
      predict = predict,
      varimp = varimp)
}


setMethod("initialize", "MLModel",
  function(.Object, ..., gridinfo = new_gridinfo()) {
    stopifnot(is_tibble(gridinfo))
    .Object@gridinfo <- new_gridinfo(
      param = gridinfo[["param"]],
      values = gridinfo[["values"]],
      regular = gridinfo[["regular"]]
    )
    .Object <- callNextMethod(.Object, ...)
    .Object
  }
)


MLModelFit <- function(object, Class, model, x) {
  model@x <- prep(x)

  if (is(object, Class)) {
    object <- unMLModelFit(object)
  } else if (is(object, "MLModelFit")) {
    stop("cannot change MLModelFit class")
  }

  if (!is(model, "MLModel")) stop("model not of class MLModel")

  if (isS4(object)) {
    object <- new(Class, object, mlmodel = model)
  } else if (is.list(object)) {
    object$mlmodel <- model
    class(object) <- c(Class, "MLModelFit", class(object))
  } else {
    stop("unsupported object class")
  }

  object
}


#' Revert an MLModelFit Object
#'
#' Function to revert an \code{MLModelFit} object to its original class.
#'
#' @param object model \link{fit} result.
#'
#' @return The supplied object with its \code{MLModelFit} classes and fields
#' removed.
#'
unMLModelFit <- function(object) {
  if (is(object, "MLModelFit")) {
    if (isS4(object)) {
      classes <- extends(class(object))
      pos <- match("MLModelFit", classes)
      as(object, classes[pos + 1])
    } else {
      object$mlmodel <- NULL
      classes <- class(object)
      pos <- match("MLModelFit", classes)
      structure(object, class = classes[-(1:pos)])
    }
  } else object
}
