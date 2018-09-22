setOldClass("recipe")


#' Resampling Classes and Methods
#'
#' @name MLControl-class
#' @rdname MLControl-class
#' 
#' @slot summary function to compute model performance metrics.
#' @slot cutoff threshold above which probabilities are classified as success
#' for factor outcomes and which expected values are rounded for integer
#' outcomes.
#' @slot cutoff_index function to calculate a desired sensitivity-specificity
#' tradeoff.
#' @slot surv_times numeric vector of follow-up times at which to predict
#' survival events.
#' @slot na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating model metrics.
#' @slot seed integer to set the seed at the start of resampling.
#' 
setClass("MLControl",
  slots = c(summary = "function", cutoff = "numeric", cutoff_index = "function",
            surv_times = "numeric", na.rm = "logical", seed = "numeric"),
  contains = "VIRTUAL"
)


#' The base MLControl constructor initializes a set of parameters that are common
#' to all resampling methods.
#' 
#' @rdname MLControl-class
#' @aliases initialize,MLControl-method
#' 
#' @param .Object class object being initialized.
#' @param summary function to compute model performance metrics.
#' @param cutoff threshold above which probabilities are classified as success
#' for factor outcomes and which expected values are rounded for integer
#' outcomes.
#' @param cutoff_index function to calculate a desired sensitivity-specificity
#' tradeoff.
#' @param surv_times numeric vector of follow-up times at which to predict
#' survival events.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating model metrics.
#' @param seed integer to set the seed at the start of resampling.  This is set
#' to a random integer by default (NULL).
#' @param ...  arguments to be passed to or from other methods.
#' 
#' @return MLControl class object.
#' 
#' @seealso \code{\link{resample}}, \code{\link{modelmetrics}}
#' 
setMethod("initialize", "MLControl",
  function(.Object, summary = modelmetrics, cutoff = 0.5,
           cutoff_index = function(sens, spec) sens + spec,
           surv_times = numeric(), na.rm = FALSE, seed = NULL, ...) {
    if(is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
    callNextMethod(.Object, summary = summary, cutoff = cutoff,
                   cutoff_index = cutoff_index, surv_times = surv_times,
                   na.rm = na.rm, seed = seed, ...)
  }
)


#' \code{BootControl} constructs an MLControl object for simple bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the full data set.
#' 
#' @name BootControl
#' @rdname MLControl-class
#' 
#' @param samples number of bootstrap samples.
#' 
BootControl <- function(samples = 25, ...) {
  new("BootControl", samples = samples, ...)
}


setClass("BootControl",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


#' \code{CVControl} constructs an MLControl object for repeated K-fold
#' cross-validation.  In this procedure, the full data set is repeatedly
#' partitioned into K-folds.  Within a partitioning, prediction is performed on each
#' of the K folds with models fit on all remaining folds.
#' 
#' @name CVControl
#' @rdname MLControl-class
#' 
#' @param folds number of cross-validation folds (K).
#' @param repeats number of repeats of the K-fold partitioning.
#' 
CVControl <- function(folds = 10, repeats = 1, ...) {
  new("CVControl", folds = folds, repeats = repeats, ...)
}


setClass("CVControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl"
)


#' \code{OOBControl} constructs an MLControl object for out-of-bag bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the unsampled cases.
#' 
#' @name OOBControl
#' @rdname MLControl-class
#' 
OOBControl <- function(samples = 25, ...) {
  new("OOBControl", samples = samples, ...)
}


setClass("OOBControl",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


#' MLModel Class Constructor
#' 
#' @param name character string name for the instantiated MLModel object.
#' @param packages character vector of packages required by the object.
#' @param types character vector of response variable types on which the model
#' can be fit.
#' @param params list of user-specified model parameters.
#' @param fit model fitting function.
#' @param predict model prediction function.
#' @param response function to extract the response variable from a model fit.
#' @param varimp variable importance function.
#' 
MLModel <- function(name = "MLModel", packages = character(0),
                    types = character(0), params = list(),
                    fit = function(...) stop("no fit function"),
                    predict = function(...) stop("no predict function"),
                    response = function(...) stop("no response function"),
                    varimp = function(...) stop("no varimp function")) {
  new("MLModel", name = name, packages = packages, types = types,
      params = params, fit = fit, predict = predict, response = response,
      varimp = varimp)
}


setClass("MLModel",
  slots = c(name = "character",
            packages = "character",
            types = "character",
            params = "list",
            fit = "function",
            predict = "function",
            response = "function",
            varimp = "function")
)


#' Model Fit Class
#' 
#' MLModelFit is the base class for model fit objects.  Direct calls to the
#' MLModelFit constructor are not necessary unless implementing custom models.
#'
#' @name MLModelFit-class
#' @rdname MLModelFit-class
#' 
#' @slot .packages character vector of packages required by the object.
#' @slot .predict model prediction function.
#' @slot .response function to extract the response variable from a model fit.
#' @slot .varimp variable importance function.
#' 
setClass("MLModelFit",
  slots = c(.packages = "character",
            .predict = "function",
            .response = "function",
            .varimp = "function"),
  contains = "VIRTUAL"
)


setClass("SVMFit", contain = c("MLModelFit", "ksvm"))
setClass("CForestFit", contains = c("MLModelFit", "RandomForest"))


#' Resamples Class Contructor
#' 
#' Create an object of resampled performance metrics from one or more models.
#' 
#' @param method character string indicating the type of resampling method.
#' This need only be specified if the supplied output is not a Resamples object.
#' @param ... named or unnamed resample output from one or more models.
#' 
#' @return Resamples class object.
#' 
#' @seealso \code{\link{resample}}, \code{\link{plot}}, \code{\link{summary}}
#' 
Resamples <- function(..., method = NULL) {
  new("Resamples", ..., method = method)  
}


setClass("Resamples",
  slots = c("method" = "character"),
  contains = "array"
)


setMethod("initialize", "Resamples",
  function(.Object, ..., method = NULL) {
    args <- list(...)
    if(length(args) == 0) stop("no values given")
    .Data <- args[[1]]
    if(length(args) == 1) {
      if(is(.Data, "Resamples")) {
        method <- .Data@method
      } else if(is.data.frame(.Data)) {
        .Data <- as.matrix(.Data)
      }
    } else {
      if(!all(sapply(args, function(x) is(x, "Resamples") && is.matrix(x)))) {
        stop("values to combine must be 2 dimensional Resamples objects")
      }
      if(!all(sapply(args, slot, name = "method") == .Data@method)) {
        stop("resamples use different methods")
      }
      if(!all(sapply(args, colnames) == colnames(.Data))) {
        stop("resamples contain different metrics")
      }
      if(!all(sapply(args, nrow) == nrow(.Data))) {
        stop("resamples have different numbers of evaluations")
      }
      method <- .Data@method
      modelnames <- names(args)
      if(is.null(modelnames)) modelnames <- paste0("Model", seq(args))
      names(args) <- NULL
      args$along <- 3
      args$new.names <- list(1:nrow(.Data), NULL, modelnames)
      .Data <- do.call(abind, args)
    }
    callNextMethod(.Object, .Data, method = method)
  }
)


MLModelTune <- setClass("MLModelTune",
  slots = c(grid = "data.frame", resamples = "Resamples", selected = "numeric"),
  contains = "MLModel"
)


ResamplesDiff <- setClass("ResamplesDiff",
  slots = c("modelnames" = "character"),
  contains = "Resamples"
)


setMethod("initialize", "ResamplesDiff",
  function(.Object, modelnames, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@modelnames <- modelnames
    .Object
  }
)


ResamplesHTest <- setClass("ResamplesHTest",
  slots = c("adjust" = "character"),
  contains = "array"
)


VarImp <- setClass("VarImp", contains = "data.frame")


setMethod("initialize", "VarImp",
  function(.Object, .Data, scale = FALSE, ...) {
    idx <- order(rowSums(.Data), decreasing = TRUE)
    idx <- idx * (rownames(.Data)[idx] != "(Intercept)")
    .Data <- .Data[idx, , drop = FALSE]
    if(scale) .Data <- 100 * (.Data - min(.Data)) / diff(range(.Data))
    callNextMethod(.Object, .Data, ...)
  }
)


setValidity("VarImp", function(object) {
  !(nrow(object) && is.null(rownames(object)))
})
