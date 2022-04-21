#' MLModel and MLModelFunction Class Constructors
#'
#' Create a model or model function for use with the \pkg{MachineShop} package.
#'
#' @param name character name of the object to which the model is assigned.
#' @param label optional character descriptor for the model.
#' @param packages character vector of package names upon which the model
#'   depends.  Each name may be optionally followed by a comment in
#'   parentheses specifying a version requirement.  The comment should contain
#'   a comparison operator, whitespace and a valid version number, e.g.
#'   \code{"xgboost (>= 1.3.0)"}.
#' @param response_types character vector of response variable types to which
#'   the model can be fit.  Supported types are \code{"binary"}, =
#'   \code{"BinomialVariate"}, \code{"DiscreteVariate"}, \code{"factor"},
#'   \code{"matrix"}, \code{"NegBinomialVariate"}, \code{"numeric"},
#'   \code{"ordered"}, \code{"PoissonVariate"}, and \code{"Surv"}.
#' @param weights logical value or vector of the same length as
#'    \code{response_types} indicating whether case weights are supported for
#'    the responses.
#' @param predictor_encoding character string indicating whether the model is
#'   fit with predictor variables encoded as a \code{"\link{model.frame}"},
#'   a \code{"\link{model.matrix}"}, or unspecified (default).
#' @param params list of user-specified model parameters to be passed to the
#'   \code{fit} function.
#' @param gridinfo tibble of information for construction of tuning grids
#'   consisting of a character column \code{param} with the names of parameters
#'   in the grid, a list column \code{get_values} with functions to generate grid
#'   points for the corresponding parameters, and an optional logical column
#'   \code{default} indicating which parameters to include by default in regular
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
#' @param ... arguments passed to other methods.
#' @param object function that returns an \code{MLModel} object when called
#'   without any supplied argument values.
#'
#' @details
#' If supplied, the \code{grid} function should return a list whose elements are
#' named after and contain values of parameters to include in a tuning grid to
#' be constructed automatically by the package.
#'
#' Arguments \code{data} and \code{newdata} in the \code{fit} and \code{predict}
#' functions may be converted to data frames with \code{as.data.frame()}
#' if needed for their operation.  The \code{fit} function should return the
#' object resulting from the model fit.  Values returned by the \code{predict}
#' functions should be formatted according to the response variable types below.
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
#' The \code{predict} and \code{varimp} functions are additionally passed a list
#' named \code{.MachineShop} containing the \code{\link[=inputs]{input}}
#' and \code{\link[=models]{model}} from \code{\link{fit}}.  This argument may
#' be included in the function definitions as needed for their implementations.
#' Otherwise, it will be captured by the ellipsis.
#'
#' @return An \code{MLModel} or \code{MLModelFunction} class object.
#'
#' @seealso \code{\link{models}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' ## Logistic regression model
#' LogisticModel <- MLModel(
#'   name = "LogisticModel",
#'   response_types = "binary",
#'   weights = TRUE,
#'   fit = function(formula, data, weights, ...) {
#'     glm(formula, data = as.data.frame(data), weights = weights,
#'         family = binomial, ...)
#'   },
#'   predict = function(object, newdata, ...) {
#'     predict(object, newdata = as.data.frame(newdata), type = "response")
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
MLModel <- function(
  name = "MLModel", label = name, packages = character(),
  response_types = character(), weights = FALSE,
  predictor_encoding = c(NA, "model.frame", "model.matrix"), params = list(),
  gridinfo = tibble::tibble(
    param = character(), get_values = list(), default = logical()
  ),
  fit = function(formula, data, weights, ...) stop("No fit function."),
  predict = function(object, newdata, times, ...) stop("No predict function."),
  varimp = function(object, ...) NULL, ...
) {

  stopifnot(!any(duplicated(response_types)))
  stopifnot(response_types %in% settings("response_types"))
  stopifnot(length(weights) %in% c(1, length(response_types)))

  stopifnot(is_tibble(gridinfo))
  gridinfo <- new_gridinfo(
    param = gridinfo[["param"]],
    get_values = gridinfo[["get_values"]],
    default = gridinfo[["default"]]
  )

  new("MLModel",
    name = name,
    label = label,
    packages = packages,
    response_types = response_types,
    weights = weights,
    predictor_encoding = match.arg(predictor_encoding),
    params = params,
    gridinfo = gridinfo,
    fit = fit,
    predict = predict,
    varimp = varimp,
    ...
  )
}


setMethod("initialize", "MLModel",
  function(.Object, ..., id = make_id("model")) {
    callNextMethod(.Object, ..., id = id)
  }
)


NullModel <- function() {
  new("NullModel", MLModel(
    id = "null",
    name = "NullModel",
    label = "Null Model",
    response_types = settings("response_types"),
    fit = function(...) {
      throw(Error("No specified model to fit."), call = call("fit"))
    }
  ))
}


update.MLModel <- function(
  object, params = NULL, quote = TRUE, new_id = FALSE, ...
) {
  old_id <- object@id
  if (is.list(params)) {
    new_params <- as(object, "list")
    new_params[names(params)] <- params
    object <- do.call(object@name, new_params, quote = quote)
  }
  if (is.character(new_id)) {
    object@id <- new_id
  } else if (!isTRUE(new_id)) {
    object@id <- old_id
  }
  object
}


#' @rdname MLModel
#'
MLModelFunction <- function(object, ...) {
  if (!is(try(object(), silent = TRUE), "MLModel")) {
    Error("Call to object() does not return an MLModel.")
  }
  new("MLModelFunction", object, ...)
}


"MLModelFunction<-" <- function(object, value) {
  do.call(MLModelFunction, c(object, value))
}


MLModelFit <- function(object, class, input, model) {
  if (is(object, "MLModelFit") && !is(object, class)) {
    throw(Error("Cannot change MLModelFit class."))
  }

  if (!is(model, "MLModel")) throw(TypeError(model, "MLModel", "`model`"))

  if (isS4(object)) {
    object <- new(class, object)
  } else {
    class <- setdiff(c(class, "MLModelFit"), class(object))
    class(object) <- c(class, class(object))
  }

  .MachineShop <- list(
    input = prep(input, retain = FALSE),
    model = model,
    version = packageVersion("MachineShop")
  )
  attr(object, ".MachineShop")[names(.MachineShop)] <- .MachineShop

  object
}


update.MLModelFit <- function(object, ...) {
  model <- NULL
  if ("mlmodel" %in% names(object)) {
    model <- object$mlmodel
    object$mlmodel <- NULL
  } else if ("mlmodel" %in% names(attributes(object))) {
    model <- attr(object, "mlmodel")
    attr(object, "mlmodel") <- NULL
  }
  if (is(model, "MLModel")) {
    .MachineShop <- list()
    y_levels <- c("y_levels" = "levels")
    add <- switch(model@name,
      "LARSModel" = "step",
      "LDAModel" = c("dimen", "use"),
      "QDAModel" = "use",
      "XGBModel" = y_levels,
      "XGBDARTModel" = y_levels,
      "XGBLinearModel" = y_levels,
      "XGBTreeModel" = y_levels
    )
    if (length(add)) {
      if (is.null(names(add))) names(add) <- add
      .MachineShop[names(add)] <- object[add]
      object[add] <- NULL
    }
    attr(object, ".MachineShop") <- .MachineShop
    input <- model@input
    if (is(input, "ModelRecipe") && is.null(input$orig_template)) {
      input$orig_template <- input$template
      input$template <- NULL
    }
    new_model <- update(model, params = model@params)
    new_model@label <- model@label
    new_model@steps <- model@steps
    MLModelFit(object, class1(object), input, new_model)
  } else {
    object
  }
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
      object <- update(object)
      attr(object, ".MachineShop") <- NULL
      classes <- class(object)
      pos <- match("MLModelFit", classes)
      structure(object, class = classes[-seq_len(pos)])
    }
  } else object
}
