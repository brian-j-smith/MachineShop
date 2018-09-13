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
#' @slot index.cutoff function to calculate a desired sensitivity-specificity
#' tradeoff.
#' @slot survtimes numeric vector of follow-up times at which to predict
#' survival events.
#' @slot seed integer to set the seed at the start of resampling.
#' 
setClass("MLControl",
  slots = c(summary = "function", cutoff = "numeric", index.cutoff = "function",
            survtimes = "numeric", seed = "numeric"),
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
#' @param index.cutoff function to calculate a desired sensitivity-specificity
#' tradeoff.
#' @param survtimes numeric vector of follow-up times at which to predict
#' survival events.
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
           index.cutoff = function(sens, spec) sens + spec,
           survtimes = numeric(), seed = NULL, ...) {
    if(is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
    callNextMethod(.Object, summary = summary, cutoff = cutoff,
                   index.cutoff = index.cutoff, survtimes = survtimes,
                   seed = seed, ...)
  }
)

setAs("MLControl", "list",
  function(from) {
    list(cutoff = from@cutoff,
         index.cutoff = from@index.cutoff,
         survtimes = from@survtimes)
  }
)


setClass("BootControl",
  slots = c(number = "numeric"),
  contains = "MLControl"
)

#' \code{BootControl} constructs an MLControl object for simple bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the full data set.
#' 
#' @name BootControl
#' @rdname MLControl-class
#' 
#' @param number number of bootstrap resamples.
#' 
BootControl <- function(number = 25, ...) {
  new("BootControl", number = number, ...)
}


setClass("CVControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl"
)

#' \code{CVControl} constructs an MLControl object for repeated K-fold
#' cross-validation.  In this procedure, the full data set is repeatedly
#' partitioned into K-folds.  Within replicates, prediction is performed on each
#' of the K folds with models fit on all remaining folds.
#' 
#' @name CVControl
#' @rdname MLControl-class
#' 
#' @param folds number of cross-validation folds (K).
#' @param repeats number of cross-validation replicates.
#' 
CVControl <- function(folds = 10, repeats = 1, ...) {
  new("CVControl", folds = folds, repeats = repeats, ...)
}


setClass("OOBControl",
  slots = c(number = "numeric"),
  contains = "MLControl"
)

#' \code{OOBControl} constructs an MLControl object for out-of-bag bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the unsampled cases.
#' 
#' @name OOBControl
#' @rdname MLControl-class
#' 
OOBControl <- function(number = 25, ...) {
  new("OOBControl", number = number, ...)
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


setMethod("show", "MLModel",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "name: ", object@name, "\n\n",
        "packages: ", paste(object@packages, collapse = ", "), "\n\n",
        "types: ", paste(object@types, collapse = ", "), "\n\n",
        "params:\n",
        sep = "")
    print(object@params)
    if(length(object@params) == 0) cat("\n")
    invisible()
  }
)


#' Resamples Class Contructor
#' 
#' Create an object of resampled performance metrics from one or more models.
#' 
#' @param ... named or unnamed resample output from one or more models.
#' 
#' @return Resamples class object.
#' 
#' @seealso \code{\link{resample}}, \code{\link{plot}}, \code{\link{summary}}
#' 
Resamples <- setClass("Resamples", contains = "array")

setMethod("initialize", "Resamples",
  function(.Object, ...) {
    args <- list(...)
    if(length(args) == 0) stop("no values given")
    .Data <- args[[1]]
    if(length(args) == 1) {
      if(is.data.frame(.Data)) .Data <- as.matrix(.Data)
    } else {
      if(!all(sapply(args, function(x) is.matrix(x) || is.data.frame(x)))) {
        stop("values must be of type matrix or data.frame")
      }
      if(!all(sapply(args, dim) == dim(.Data))) {
        stop("values have different dimensions")
      }
      if(!all(sapply(args, colnames) == colnames(.Data))) {
        stop("values have different column names")
      }
      modelnames <- names(args)
      if(is.null(modelnames)) modelnames <- paste0("Model", seq(args))
      names(args) <- NULL
      args$along = 3
      args$new.names = list(1:nrow(.Data), NULL, modelnames)
      .Data <- do.call(abind, args)
    }
    callNextMethod(.Object, .Data)
  }
)


setMethod("show", "Resamples",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n", sep = "")
    dns <- dimnames(object)
    if(length(dns) > 2) cat("models: ", paste(dns[[3]], collapse = ", "),
                            "\n\n", sep = "")
    cat("metrics: ", paste(dns[[2]], collapse = ", "), "\n\n",
        "resamples: ", dim(object)[1], "\n\n", sep = "")
    invisible()
  }
)


MLModelTune <- setClass("MLModelTune",
  slots = c(grid = "data.frame", resamples = "Resamples", selected = "numeric"),
  contains = "MLModel"
)


setMethod("show", "MLModelTune",
  function(object) {
    callNextMethod(object)
    cat("grid:\n")
    print(object@grid)
    cat("\nresamples:\n")
    print(object@resamples)
    if(length(dim(object@resamples)) > 2) {
      cat("selected: Model", object@selected, " (", names(object@selected),
          ")\n\n", sep = "")
    }
  }
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


setMethod("show", "ResamplesHTest",
  function(object) {
    cat("An object of class \"", class(object), "\"\n\n",
        "upper diagonal: mean differences (row - column)\n",
        "lower diagonal: p-values\n",
        "p-value adjustment: ", object@adjust, "\n\n",
        sep = "")
    print(object@.Data)
  }
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
  contains ="VIRTUAL"
)

asMLModelFit <- function(object, Class, model = NULL) {
  if(isS4(object)) {
    object <- as(object, Class)
    if(!inherits(object, "MLModelFit")) stop("Class not from MLModelFit")
  } else if(is.list(object)) {
    class(object) <- c(Class, "MLModelFit", class(object))
  } else {
    stop("unsupported object class")
  }
  if(!is.null(model)) {
    if(!inherits(model, "MLModel")) stop("model not of class MLModel")
    field(object, ".packages") <- model@packages
    field(object, ".predict") <- model@predict
    field(object, ".response") <- model@response
    field(object, ".varimp") <- model@varimp
  }
  object
}

asParentFit <- function(object) {
  if(!inherits(object, "MLModelFit")) stop("object not of class MLModelFit")
  if(isS4(object)) {
    classes <- extends(class(object))
    from <- match("MLModelFit", classes) + 1
    as(object, classes[from])
  } else {
    object[c(".packages", ".predict", ".response", ".varimp")] <- NULL
    classes <- class(object)
    from <- match("MLModelFit", classes) + 1
    to <- length(classes)
    structure(object, class = classes[from:to])
  }
}


setClass("SVMFit", contain = c("MLModelFit", "ksvm"))
setClass("CForestFit", contains = c("MLModelFit", "RandomForest"))


setClass("VarImp", contains = "data.frame")

setMethod("initialize", "VarImp",
  function(.Object, .Data, scale = FALSE, ...) {
    idx <- order(rowSums(.Data), decreasing = TRUE)
    idx <- idx * (rownames(.Data)[idx] != "(Intercept)")
    .Data <- .Data[idx, , drop = FALSE]
    if(scale) .Data <- 100 * (.Data - min(.Data)) / diff(range(.Data))
    callNextMethod(.Object, .Data, ...)
  }
)

setAs("data.frame", "VarImp",
  function(from) new("VarImp", from)
)

setAs("matrix", "VarImp",
  function(from) as(as.data.frame(from), "VarImp")
)

setAs("vector", "VarImp",
  function(from) as(data.frame(Overall = from), "VarImp")
)

setValidity("VarImp", function(object) {
  !(nrow(object) && is.null(rownames(object)))
})
