setOldClass("ModelFrame")
setOldClass("recipe")


#' Resampling Controls
#' 
#' @description
#' The base \code{MLControl} constructor initializes a set of control parameters
#' that are common to all resampling methods.
#' 
#' @rdname MLControl
#' 
#' @param summary function to compute model performance metrics (deprecated).
#' @param cutoff threshold above which probabilities are classified as success
#' for factor outcomes and which expected values are rounded for integer
#' outcomes (deprecated).
#' @param cutoff_index function to calculate a desired sensitivity-specificity
#' tradeoff (deprecated).
#' @param surv_times numeric vector of follow-up times at which to predict
#' survival events.
#' @param na.rm logical indicating whether to remove observed or predicted
#' responses that are \code{NA} when calculating model metrics (deprecated).
#' @param seed integer to set the seed at the start of resampling.  This is set
#' to a random integer by default (NULL).
#' @param ...  arguments to be passed to \code{MLControl}.
#' 
#' @details
#' Arguments \code{summary}, \code{cutoff}, \code{cutoff_index}, and
#' \code{na.rm} are deprecated.  The latter three may be specified directly in
#' calls to \code{\link{modelmetrics}} instead.
#' 
#' @return \code{MLControl} class object.
#' 
#' @seealso \code{\link{resample}}, \code{\link{modelmetrics}}
#' 
MLControl <- function(summary = NULL, cutoff = NULL, cutoff_index = NULL,
                      surv_times = numeric(), na.rm = NULL, seed = NULL) {
    if (!is.null(summary)) {
      depwarn("'summary' argument to MLControl is deprecated",
              "apply the modelmetrics function to Resamples output directly")
    }
    
    if (!is.null(cutoff)) {
      depwarn("'cutoff' argument to MLContorl is deprecated",
              "specify in calls to modelmetrics instead")
    }
    
    if (!is.null(cutoff_index)) {
      depwarn("'cutoff_index' argument to MLControl is deprecated",
              "specify in calls to modelmetrics instead")
    }
    
    if (!is.null(na.rm)) {
      depwarn("'na.rm' argument to MLControl is deprecated",
              "specify in calls to modelmetrics instead")
    }
    
    if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
    new("MLControl", surv_times = surv_times, na.rm = TRUE, seed = seed)
}


setClass("MLControl",
  slots = c(summary = "function", cutoff = "numeric", cutoff_index = "function",
            surv_times = "numeric", na.rm = "logical", seed = "numeric")
)


#' @description
#' \code{BootControl} constructs an \code{MLControl} object for simple bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the full data set.
#' 
#' @rdname MLControl
#' 
#' @param samples number of bootstrap samples.
#' 
#' @examples
#' ## 100 bootstrap samples
#' BootControl(samples = 100)
#' 
BootControl <- function(samples = 25, ...) {
  new("BootMLControl", MLControl(...), samples = samples)
}


setClass("BootMLControl",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


#' @description
#' \code{CVControl} constructs an \code{MLControl} object for repeated K-fold
#' cross-validation.  In this procedure, the full data set is repeatedly
#' partitioned into K-folds.  Within a partitioning, prediction is performed on
#' each of the K folds with models fit on all remaining folds.
#' 
#' @rdname MLControl
#' 
#' @param folds number of cross-validation folds (K).
#' @param repeats number of repeats of the K-fold partitioning.
#' 
#' @examples
#' ## 5 repeats of 10-fold cross-validation
#' CVControl(folds = 10, repeats = 5)
#' 
CVControl <- function(folds = 10, repeats = 1, ...) {
  new("CVMLControl", MLControl(...), folds = folds, repeats = repeats)
}


setClass("CVMLControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl"
)


#' @description
#' \code{OOBControl} constructs an \code{MLControl} object for out-of-bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the unsampled cases.
#' 
#' @rdname MLControl
#' 
#' @examples
#' ## 100 out-of-bootstrap samples
#' OOBControl(samples = 100)
#' 
OOBControl <- function(samples = 25, ...) {
  new("OOBMLControl", MLControl(...), samples = samples)
}


setClass("OOBMLControl",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


#' @description
#' \code{SplitControl} constructs an \code{MLControl} object for splitting data
#' into a seperate trianing and test set.
#' 
#' @rdname MLControl
#' 
#' @param prop proportion of cases to include in the training set
#' (\code{0 < prop < 1}).
#' 
#' @examples
#' ## Split sample of 2/3 training and 1/3 testing
#' SplitControl(prop = 2/3)
#' 
SplitControl <- function(prop = 2/3, ...) {
  new("SplitMLControl", MLControl(...), prop = prop)
}


setClass("SplitMLControl",
  slots = c(prop = "numeric"),
  contains = "MLControl"
)


#' @description
#' \code{TrainControl} constructs an \code{MLControl} object for training and
#' performance evaluation to be performed on the same training set.
#' 
#' @rdname MLControl
#' 
#' @examples
#' ## Same training and test set
#' TrainControl()
#' 
TrainControl <- function(...) {
  new("TrainMLControl", MLControl(...))
}


setClass("TrainMLControl",
  contains = "MLControl"
)


MLFitBits <- setClass("MLFitBits",
  slots = c(packages = "character",
            predict = "function",
            varimp = "function",
            x = "ANY",
            y = "ANY")
)


#' MLModel Class Constructor
#' 
#' @param name character string name for the instantiated \code{MLModel} object.
#' @param packages character vector of packages required by the object.
#' @param types character vector of response variable types on which the model
#' can be fit.
#' @param params list of user-specified model parameters.
#' @param nvars function to return the number of predictor variables for a
#' given model frame.
#' @param fit model fitting function.
#' @param predict model prediction function.
#' @param varimp variable importance function.
#' 
MLModel <- function(name = "MLModel", packages = character(0),
                    types = character(0), params = list(),
                    nvars = function(data) NULL,
                    fit = function(formula, data, weights, ...)
                      stop("no fit function"),
                    predict = function(object, newdata, times, ...)
                      stop("no predict function"),
                    varimp = function(object, ...) NULL) {
  
  stopifnot(types %in% c("binary", "factor", "matrix", "numeric", "ordered",
                         "Surv"))
  
  new("MLModel",
      name = name,
      packages = packages,
      types = types,
      params = params,
      nvars = nvars,
      fit = fit,
      fitbits = MLFitBits(packages = packages,
                          predict = predict,
                          varimp = varimp))
}


setClass("MLModel",
  slots = c(name = "character",
            packages = "character",
            types = "character",
            params = "list",
            nvars = "function",
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


#' Resamples Class Contructor
#' 
#' Create an object of resampled performance metrics from one or more models.
#' 
#' @param control \code{MLControl} object used to generate the resample output.
#' @param strata character string indicating the strata variable, if any.
#' @param ... named or unnamed resample output from one or more models.
#' 
#' @details Argument \code{control} need only be specified if the supplied
#' output is not a \code{Resamples} object.  Output being combined from more
#' than one model must have been generated with the same resampling control
#' object.
#' 
#' @return \code{Resamples} class object.
#' 
#' @seealso \code{\link{resample}}, \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- Species ~ .
#' control <- CVControl()
#' 
#' gbmres1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbmres2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbmres3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' 
#' res <- Resamples(GBM1 = gbmres1, GBM2 = gbmres2, GBM3 = gbmres3)
#' summary(res)
#' plot(res)
#' 
Resamples <- function(..., control = NULL, strata = character()) {
  new("Resamples", ..., control = control, strata = strata)
}


setClass("Resamples",
  slots = c(control = "MLControl", strata = "character"),
  contains = "data.frame"
)


setMethod("initialize", "Resamples",
  function(.Object, ..., control, strata) {
    args <- list(...)
    
    if (length(args) == 0) stop("no resample output given")
    
    .Data <- args[[1]]
    if (length(args) == 1) {
      if (is(.Data, "Resamples")) {
        strata <- .Data@strata
      }
    } else {
      if (!all(sapply(args, function(x) is(x, "Resamples")))) {
        stop("values to combine must be Resamples objects")
      }
      
      if (!all(sapply(args, function(x) nlevels(x$Model) == 1))) {
        stop("resamples must be from single models")
      }

      control <- .Data@control
      is_equal_control <- function(x) isTRUE(all.equal(x@control, control))
      if (!all(sapply(args, is_equal_control))) {
        stop("resamples have different control structures")
      }
      
      strata <- .Data@strata
      if (!all(sapply(args, function(x) x@strata == strata))) {
        stop("resamples have different strata variables")
      }

      .Data <- Reduce(append, args)

      old_model_names <- sapply(args, function(x) levels(x$Model))
      model_names <- names(args)
      if (is.null(model_names)) {
        model_names <- old_model_names
      } else {
        model_names <- ifelse(nzchar(model_names), model_names, old_model_names)
      }
      model_names <- make.names(model_names, unique = TRUE)
      num_times <- sapply(args, nrow)
      .Data$Model <- factor(rep(model_names, num_times), levels = model_names)
    }
    
    resample_vars <- c("Model", "Resample", "Case", "Observed", "Predicted")
    is_missing <- !(resample_vars %in% names(.Data))
    if (any(is_missing)) {
      stop("missing resample variables: ", toString(resample_vars[is_missing]))
    }
    
    callNextMethod(.Object, .Data, control = control,
                   strata = as.character(strata))
  }
)


MLModelTune <- setClass("MLModelTune",
  slots = c(grid = "data.frame", resamples = "Resamples", selected = "numeric"),
  contains = "MLModel"
)


Calibration <- function(...) {
  args <- make_unique_levels(list(...), which = "Model")
  structure(do.call(rbind, args), class = c("Calibration", "data.frame"))
}


Confusion <- function(...) {
  structure(list(...), class = c("Confusion", "listof"))
}


ConfusionMatrix <- function(object) {
   structure(object, class = c("ConfusionMatrix", "table"))
}


HTestResamples <- setClass("HTestResamples",
  slots = c("adjust" = "character"),
  contains = "array"
)


Lift <- function(...) {
  args <- make_unique_levels(list(...), which = "Model")
  structure(do.call(rbind, args), class = c("Lift", "data.frame"))
}


ModelMetrics <- setClass("ModelMetrics",
  contains = "array"
)


ModelMetricsDiff <- setClass("ModelMetricsDiff",
  slots = c("model_names" = "character"),
  contains = "ModelMetrics"
)


PartialDependence <- function(object) {
  structure(object, class = c("PartialDependence", "data.frame"))
}


SummaryConfusion <- setClass("SummaryConfusion",
  slots = c("N" = "numeric", "Accuracy" = "numeric", "Majority" = "numeric",
            "Kappa" = "numeric"),
  contains = "matrix"
)


VarImp <- setClass("VarImp", contains = "data.frame")


setMethod("initialize", "VarImp",
  function(.Object, .Data, scale = FALSE, ...) {
    idx <- order(rowSums(.Data), decreasing = TRUE)
    idx <- idx * (rownames(.Data)[idx] != "(Intercept)")
    .Data <- .Data[idx, , drop = FALSE]
    if (scale) .Data <- 100 * (.Data - min(.Data)) / diff(range(.Data))
    callNextMethod(.Object, .Data, ...)
  }
)


setValidity("VarImp", function(object) {
  !(nrow(object) && is.null(rownames(object)))
})
