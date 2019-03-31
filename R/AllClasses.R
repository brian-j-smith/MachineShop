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


setClass("HTestPerformanceDiff",
  slots = c(adjust = "character"),
  contains = "array"
)


setClass("Lift",
  contains = "Curves"
)


setClass("Performance",
  contains = "array"
)


setClass("PerformanceDiff",
  slots = c(model_names = "character"),
  contains = "Performance"
)


setClass("MLModelTune",
  slots = c(tune_grid = "data.frame",
            performance = "Performance",
            selected = "numeric"),
  contains = "MLModel"
)


setClass("Resamples",
  slots = c(control = "MLControl",
            strata = "character"),
  contains = "data.frame"
)


setClass("SummaryConfusion",
  slots = c(N = "numeric",
            Accuracy = "numeric",
            Majority = "numeric",
            Kappa = "numeric"),
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


VarImp <- function(object, scale = FALSE) {
  stopifnot(nrow(object) == 0 || is.character(rownames(object)))

  idx <- order(rowSums(object), decreasing = TRUE)
  idx <- idx * (rownames(object)[idx] != "(Intercept)")
  object <- object[idx, , drop = FALSE]
  if (scale) object <- 100 * (object - min(object)) / diff(range(object))
  
  new("VarImp", object)
}


setClass("VarImp", contains = "data.frame")
