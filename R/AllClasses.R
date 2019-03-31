setOldClass("ModelFrame")
setOldClass("recipe")
setOldClass("SurvEvents")
setOldClass("SurvMatrix")
setOldClass("SurvProbs")


setClass("MLControl",
  slots = c(times = "ANY",
            dist = "ANY",
            method = "ANY",
            seed = "numeric")
)


setClass("MLControlBoot",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


setClass("MLControlCV",
  slots = c(folds = "numeric",
            repeats = "numeric"),
  contains = "MLControl"
)


setClass("MLControlOOB",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


setClass("MLControlSplit",
  slots = c(prop = "numeric"),
  contains = "MLControl"
)


setClass("MLControlTrain",
  contains = "MLControl"
)


MLFitBits <- setClass("MLFitBits",
  slots = c(packages = "character",
            predict = "function",
            varimp = "function",
            x = "ANY",
            y = "ANY")
)


setClass("MLMetric",
  slots = c(name = "character",
            label = "character",
            maximize = "logical"),
  contains = "function"
)


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


setClass("MLModel",
  slots = c(name = "character",
            label = "character",
            packages = "character",
            types = "character",
            params = "list",
            grid = "function",
            design = "character",
            fit = "function",
            fitbits = "MLFitBits")
)


setClass("MLModelFit",
  slots = c(fitbits = "MLFitBits"),
  contains = "VIRTUAL"
)


setClass("SVMModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMANOVAModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMBesselModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMLaplaceModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMLinearModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMPolyModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMRadialModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMSplineModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMTanhModelFit", contain = c("MLModelFit", "ksvm"))
setClass("CForestModelFit", contains = c("MLModelFit", "RandomForest"))


#' @name calibration
#' @rdname calibration
#' 
#' @param ... named or unnamed \code{calibration} output to combine together
#' with the \code{Calibration} constructor.
#' 
Calibration <- function(...) {
  .Calibration(...)
}


.Calibration <- function(..., .breaks) {
  args <- list(...)
  
  if (length(args) == 0) stop("no calibration output given")
  
  .Data <- args[[1]]
  if (all(mapply(is, args, "Calibration"))) {
    
    smoothed <- .Data@smoothed
    if (!all(sapply(args, function(x) identical(x@smoothed, smoothed)))) {
      stop("Calibration arguments are a mix of smoothed and binned curves")
    }

  } else if (length(args) > 1) {
    
    stop("arguments to combine must be Calibration objects")
    
  } else if (!is.data.frame(.Data)) {
    
    stop("Calibration argument must inherit from data.frame")
    
  } else {

    if (missing(.breaks)) stop("missing breaks in Calibration constructor")
    smoothed <- is.null(.breaks)

    var_names <- c("Response", "Predicted", "Observed")
    is_missing <- !(var_names %in% names(.Data))
    if (any(is_missing)) {
      stop("missing calibration variables: ", toString(var_names[is_missing]))
    }
    
  }

  args <- make_unique_levels(args, which = "Model")
  new("Calibration", do.call(append, args), smoothed = smoothed)
}


setClass("Calibration",
  slots = c("smoothed" = "logical"),
  contains = "data.frame"
)


#' @name confusion
#' @rdname confusion
#' 
#' @param ... named or unnamed \code{confusion} output to combine together with
#' the \code{Confusion} constructor.
#' 
Confusion <- function(...) {
  args <- list(...)
  
  conf_list <- list()
  for (i in seq(args)) {
    x <- args[[i]]
    if (is(x, "ConfusionMatrix")) {
      x <- list("Model" = x)
    } else if (!is(x, "Confusion")) {
      stop("values to combine must be Confusion or ConfusionMatrix objects")
    }
    arg_name <- names(args)[i]
    if (!is.null(arg_name) && nzchar(arg_name)) {
      names(x) <- rep(arg_name, length(x))
    }
    conf_list <- c(conf_list, x)
  }
  names(conf_list) <- make.unique(names(conf_list))

  structure(conf_list, class = c("Confusion", "listof"))
}


ConfusionMatrix <- setClass("ConfusionMatrix",
  contains = c("table", "matrix")
)


#' @name performance_curve
#' @rdname performance_curve
#' 
#' @param ... named or unnamed \code{performance_curve} output to combine
#' together with the \code{Curves} constructor.
#' 
Curves <- function(...) {
  .Curves(...)
}


.Curves <- function(..., .metrics = list()) {
  args <- list(...)
  
  if (length(args) == 0) stop("no performance_curve output given")
  
  .Data <- args[[1]]
  if (all(mapply(is, args, "Curves"))) {
    
    metrics <- .Data@metrics
    if (!all(sapply(args, function(x) identical(x@metrics, metrics)))) {
      stop("Curves arguments have different metrics")
    }

  } else if (length(args) > 1) {
    
    stop("arguments to combine must be Curves objects")
    
  } else if (!is.data.frame(.Data)) {
    
    stop("Curves argument must inherit from data.frame")
    
  } else {
    
    if (!all(mapply(is, .metrics[1:2], "MLMetric"))) {
      stop("missing performance metrics in Curves constructor")
    }
    metrics <- c(y = .metrics[[1]], x = .metrics[[2]])

    var_names <- c("Cutoff", "x", "y")
    is_missing <- !(var_names %in% names(.Data))
    if (any(is_missing)) {
      stop("missing performance curve variables: ",
           toString(var_names[is_missing]))
    }
    
    decreasing <- !xor(metrics$x@maximize, metrics$y@maximize)
    sort_order <- order(.Data$x, .Data$y, decreasing = c(FALSE, decreasing),
                        method = "radix")
    args[[1]] <- .Data[sort_order, , drop = FALSE]

  }

  args <- make_unique_levels(args, which = "Model")
  new("Curves", do.call(append, args), metrics = metrics)
}


setClass("Curves",
  slots = c("metrics" = "list"),
  contains = "data.frame"
)


HTestPerformanceDiff <- setClass("HTestPerformanceDiff",
  slots = c("adjust" = "character"),
  contains = "array"
)


#' @name lift
#' @rdname lift
#' 
#' @param ... named or unnamed \code{lift} output to combine together with the
#' \code{Lift} constructor.
#' 
Lift <- function(...) {
  object <- as(Curves(...), "Lift")
  if (!all(mapply(identical, object@metrics, c(tpr, rpp)))) {
    stop("incorrect lift metrics")
  }
  object
}


setClass("Lift",
  contains = "Curves"
)


PartialDependence <- function(object) {
  structure(object, class = c("PartialDependence", "data.frame"))
}


Performance <- function(...) {
  args <- list(...)
  
  perf <- if (length(args) > 1) {
    abind(args, along = 3)
  } else {
    args[[1]]
  }
  
  new("Performance", perf)
}


setClass("Performance",
  contains = "array"
)


PerformanceDiff <- setClass("PerformanceDiff",
  slots = c("model_names" = "character"),
  contains = "Performance"
)


MLModelTune <- setClass("MLModelTune",
  slots = c(tune_grid = "data.frame",
            performance = "Performance",
            selected = "numeric"),
  contains = "MLModel"
)


#' @name resample
#' @rdname resample-methods
#' 
#' @param ... named or unnamed \code{resample} output to combine together with
#' the \code{Resamples} constructor.
#' 
#' @details Output being combined from more than one model with the
#' \code{Resamples} constructor must have been generated with the same
#' resampling \code{control} object.
#' 
Resamples <- function(...) {
  .Resamples(...)
}


.Resamples <- function(..., .control = NULL, .strata = NULL) {
  args <- list(...)
  
  if (length(args) == 0) stop("no resample output given")
  
  .Data <- args[[1]]
  if (all(mapply(is, args, "Resamples"))) {
    
    control <- .Data@control
    if (!all(sapply(args, function(x) identical(x@control, control)))) {
      stop("Resamples arguments have different control structures")
    }

    strata <- .Data@strata
    if (!all(sapply(args, function(x) identical(x@strata, strata)))) {
      stop("Resamples arguments have different strata variables")
    }

  } else if (length(args) > 1) {
    
    stop("arguments to combine must be Resamples objects")
    
  } else if (!is.data.frame(.Data)) {
    
    stop("Resamples argument must inherit from data.frame")
    
  } else {

    control <- .control
    if (!is(control, "MLControl")) {
      stop("missing control structure in Resamples constructor")
    }
    
    strata <- as.character(.strata)

    var_names <- c("Resample", "Case", "Observed", "Predicted")
    is_missing <- !(var_names %in% names(.Data))
    if (any(is_missing)) {
      stop("missing resample variables: ", toString(var_names[is_missing]))
    }
    
  }

  args <- make_unique_levels(args, which = "Model")
  new("Resamples", do.call(append, args), control = control, strata = strata)
}


setClass("Resamples",
  slots = c(control = "MLControl", strata = "character"),
  contains = "data.frame"
)


#' Extract Parts of an Object
#' 
#' Operators acting on data structures to extract parts.
#' 
#' @name extract
#' @rdname extract-methods
#' @aliases [,Resamples,ANY,ANY,ANY-method
#' 
#' @param x object from which to extract elements.
#' @param i,j indices specifying elements to extract.
#' @param drop logical indicating that the result be returned as a
#' \code{numeric} coerced to the lowest dimension possible if \code{TRUE} or
#' retained as the original 2-dimensional object class otherwise.
#' 
#' @seealso \code{\link{resample}}
#' 
setMethod("[", c(x = "Resamples", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, drop = FALSE) {
    if (drop) {
      callNextMethod()
    } else {
      j <- TRUE
      new("Resamples", callNextMethod(), control = x@control, strata = x@strata)
    }
  }
)


SummaryConfusion <- setClass("SummaryConfusion",
  slots = c("N" = "numeric", "Accuracy" = "numeric", "Majority" = "numeric",
            "Kappa" = "numeric"),
  contains = "matrix"
)


SurvMatrix <- function(object, times = NULL) {
  object <- as.matrix(object)
  
  if (is.null(times)) times <- rep(NA_real_, ncol(object))
  
  if (length(times) != ncol(object)) {
    stop("unequal number of survival times and predictions")
  }
  
  dimnames(object) <- list(NULL, paste("Time", seq(ncol(object))))
  
  structure(object, class = "SurvMatrix", times = times)
}


#' SurvMatrix Class Constructor
#' 
#' Create an object of predicted survival events or probabilites for use with
#' metrics provided by the \pkg{MachineShop} package.
#' 
#' @name SurvMatrix
#' @rdname SurvMatrix
#' 
#' @param object matrix, or object that can be converted to one, of predicted
#' survival events or probabilities with columns and rows representing
#' prediction times and cases, respectively.
#' @param times numeric vector of the survival prediction times.
#' 
#' @return Object that is of the same class as the constructor name and inherits
#' from \code{SurvMatrix}.  Examples of these objects are the predicted survival
#' events and probabilities returned by the \code{predict} function.
#' 
#' @seealso \code{\link{metrics}}, \code{\link{predict}}
#' 
SurvEvents <- function(object = numeric(), times = NULL) {
  object <- SurvMatrix(object, times)
  structure(object, class = c("SurvEvents", class(object)))
}


#' @rdname SurvMatrix
#' 
SurvProbs <- function(object = numeric(), times = NULL) {
  object <- SurvMatrix(object, times)
  structure(object, class = c("SurvProbs", class(object)))
}


#' @rdname extract-methods
#' @aliases [.SurvMatrix
#' 
#' @seealso \code{\link{SurvMatrix}}
#' 
"[.SurvMatrix" <- function(x, i, j, drop = FALSE) {
  y <- unclass(x)[i, j, drop = drop]
  if (drop) y else structure(y, class = class(x), times = time(x)[j])
}


VarImp <- function(object, scale = FALSE) {
  stopifnot(nrow(object) == 0 || is.character(rownames(object)))

  idx <- order(rowSums(object), decreasing = TRUE)
  idx <- idx * (rownames(object)[idx] != "(Intercept)")
  object <- object[idx, , drop = FALSE]
  if (scale) object <- 100 * (object - min(object)) / diff(range(object))
  
  new("VarImp", object)
}


setClass("VarImp", contains = "data.frame")
