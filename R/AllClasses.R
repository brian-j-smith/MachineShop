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


setClass("MLFitBits",
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


setClass("Calibration",
  slots = c(smoothed = "logical"),
  contains = "data.frame"
)


setClass("ConfusionMatrix",
  contains = c("table", "matrix")
)


setClass("Curves",
  slots = c(metrics = "list"),
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
