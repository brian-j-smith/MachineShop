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
#' @param surv_times numeric vector of follow-up times at which to predict
#' survival events.
#' @param seed integer to set the seed at the start of resampling.  This is set
#' to a random integer by default (NULL).
#' @param ...  arguments to be passed to \code{MLControl}.
#' 
#' @return \code{MLControl} class object.
#' 
#' @seealso \code{\link{resample}}
#' 
MLControl <- function(surv_times = numeric(), seed = NULL, ...) {
  MLControl_depwarn(...)
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
  new("MLControl", surv_times = surv_times, seed = seed)
}


MLControl_depwarn <- function(summary = NULL, cutoff = NULL,
                              cutoff_index = NULL, na.rm = NULL, ...) {
  if (!is.null(summary)) {
    depwarn("'summary' argument to MLControl is deprecated",
            "apply the 'performance' function to Resamples output directly")
  }
  
  if (!is.null(cutoff)) {
    depwarn("'cutoff' argument to MLContorl is deprecated",
            "specify in calls to 'performance' instead")
  }
  
  if (!is.null(cutoff_index)) {
    depwarn("'cutoff_index' argument to MLControl is deprecated",
            "specify in calls to 'performance' instead")
  }
  
  if (!is.null(na.rm)) {
    depwarn("'na.rm' argument to MLControl is deprecated",
            "specify in calls to 'performance' instead")
  }
}


setClass("MLControl",
  slots = c(surv_times = "numeric", seed = "numeric")
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
  new("MLControlBoot", MLControl(...), samples = samples)
}


setClass("MLControlBoot",
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
  new("MLControlCV", MLControl(...), folds = folds, repeats = repeats)
}


setClass("MLControlCV",
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
  new("MLControlOOB", MLControl(...), samples = samples)
}


setClass("MLControlOOB",
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
  new("MLControlSplit", MLControl(...), prop = prop)
}


setClass("MLControlSplit",
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
  new("MLControlTrain", MLControl(...))
}


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


#' MLMetric Class Constructor
#' 
#' Create a performance metric for use with the \pkg{MachineShop} package.
#' 
#' @rdname MLMetric
#' 
#' @param object function to compute the metric.  Must be defined to accept
#' \code{observed} and \code{predicted} as the first two arguments and with an
#' ellipsis (\code{...}) to accommodate others.
#' @param name character string name for the instantiated \code{MLMetric}
#' object; same as the metric function name.
#' @param label descriptive label for the metric.
#' @param maximize logical indicating whether to maximize the metric for better
#' performance.
#' 
#' @return \code{MLMetric} class object.
#' 
#' @seealso \code{\link{metrics}}, \code{\link{metricinfo}}
#' 
MLMetric <- function(object, name = "MLMetric", label = name, maximize = TRUE) {
  new("MLMetric", object, name = name, label = label, maximize = maximize)
}


#' @rdname MLMetric
#' 
#' @param value list of arguments to pass to the \code{MLMetric} constructor.
#' 
#' @examples
#' f2_score <- function(observed, predicted, ...) {
#'   f_score(observed, predicted, beta = 2, ...)
#' }
#' 
#' MLMetric(f2_score) <- list(name = "f2_score",
#'                            label = "F Score (beta = 2)",
#'                            maximize = TRUE)
#' 
"MLMetric<-" <- function(object, value) {
  do.call(MLMetric, c(object, value))
}


setClass("MLMetric",
  slots = c(name = "character", label = "character", maximize = "logical"),
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
#' @param design character string indicating whether the type of design matrix
#' used to fit the model is a \code{"\link{model.matrix}"}, a data.frame
#' of the original predictor variable \code{"terms"}, or unknown (default).
#' @param fit model fitting function whose arguments are a \code{formula}, a
#' \code{data} frame, case \code{weights}, and an ellipsis (\code{...}).
#' @param predict model prediction function whose arguments are the
#' \code{object} returned by \code{fit}, a \code{newdata} frame of predictor
#' variables, optional vector of \code{times} at which to predict survival,
#' and an ellipsis.
#' @param varimp variable importance function whose arguments are the
#' \code{object} returned by \code{fit}, optional arguments passed from calls
#' to \code{\link{varimp}}, and an ellipsis.
#' @param ... arguments passed from other methods.
#' 
#' @details
#' Values returned by the \code{predict} functions should be formatted according
#' to the response variable types below.
#' \describe{
#' \item{factor}{a vector or column matrix of probabilities for the second level
#' of binary factors or a matrix whose columns contain the probabilities for
#' factors with more than two levels.}
#' \item{matrix}{a matrix of predicted responses.}
#' \item{numeric}{a vector or column matrix of predicted responses.}
#' \item{Surv}{a matrix whose columns contain survival probabilities at
#' \code{times} if supplied or a vector of survival predictions otherwise.}
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
                    design = c(NA, "model.matrix", "terms"),
                    fit = function(formula, data, weights, ...)
                      stop("no fit function"),
                    predict = function(object, newdata, times, ...)
                      stop("no predict function"),
                    varimp = function(object, ...) NULL, ...) {
  
  stopifnot(types %in% c("binary", "factor", "matrix", "numeric", "ordered",
                         "Surv"))
  
  MLModel_depwarn(...)
  
  new("MLModel",
      name = name,
      label = label,
      packages = packages,
      types = types,
      params = params,
      design = match.arg(design),
      fit = fit,
      fitbits = MLFitBits(packages = packages,
                          predict = predict,
                          varimp = varimp))
}


MLModel_depwarn <- function(nvars = NULL, ...) {
  if (!is.null(nvars)) {
    depwarn("'nvars' argument to MLModel is deprecated",
            "specify the design matrix type with 'design' instead")
  }
}


setClass("MLModel",
  slots = c(name = "character",
            label = "character",
            packages = "character",
            types = "character",
            params = "list",
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
  args <- list(...)
  
  if (!all(sapply(args, is.data.frame))) {
    stop("values to combine must inherit from data.frame")
  }
  
  var_names <- c("Response", "Predicted", "Observed")
  for (x in args) {
    is_missing <- !(var_names %in% names(x))
    if (any(is_missing)) {
      stop("missing calibration variables: ", toString(var_names[is_missing]))
    }
  }

  args <- make_unique_levels(args, which = "Model")
  new("Calibration", do.call(append, args))
}


setClass("Calibration",
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


ConfusionMatrix <- function(object) {
   structure(object, class = c("ConfusionMatrix", "table"))
}


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
  args <- list(...)
  
  if (!all(sapply(args, is.data.frame))) {
    stop("values to combine must inherit from data.frame")
  }
  
  var_names <- c("Found", "Tested")
  for (x in args) {
    is_missing <- !(var_names %in% names(x))
    if (any(is_missing)) {
      stop("missing lift variables: ", toString(var_names[is_missing]))
    }
  }

  args <- make_unique_levels(args, which = "Model")
  new("Lift", do.call(append, args))
}


setClass("Lift",
  contains = "data.frame"
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


.Resamples <- function(..., .control = NULL, .strata = character()) {
  args <- list(...)
  
  if (length(args) == 0) stop("no resample output given")
  
  .Data <- args[[1]]
  if (length(args) > 1) {
    if (!all(sapply(args, function(x) is(x, "Resamples")))) {
      stop("values to combine must be Resamples objects")
    }
    
    .control <- .Data@control
    is_equal_control <- function(x) isTRUE(all.equal(x@control, .control))
    if (!all(sapply(args, is_equal_control))) {
      stop("resamples have different control structures")
    }
    
    .strata <- .Data@strata
    if (!all(sapply(args, function(x) x@strata == .strata))) {
      stop("resamples have different strata variables")
    }
    
    .Data <- do.call(append, make_unique_levels(args, which = "Model"))
  }
  
  var_names <- c("Model", "Resample", "Case", "Observed", "Predicted")
  is_missing <- !(var_names %in% names(.Data))
  if (any(is_missing)) {
    stop("missing resample variables: ", toString(var_names[is_missing]))
  }
  
  new("Resamples", .Data, control = .control, strata = as.character(.strata))
}


setClass("Resamples",
  slots = c(control = "MLControl", strata = "character"),
  contains = "data.frame"
)


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
