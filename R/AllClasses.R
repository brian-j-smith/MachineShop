setOldClass("ModelFrame")
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
           surv_times = numeric(), na.rm = TRUE, seed = NULL, ...) {
    if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
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
#' @examples
#' ## 100 bootstrap samples
#' BootControl(samples = 100)
#' 
BootControl <- function(samples = 25, ...) {
  new("BootMLControl", samples = samples, ...)
}


setClass("BootMLControl",
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
#' @examples
#' ## 5 repeats of 10-fold cross-validation
#' CVControl(folds = 10, repeats = 5)
#' 
CVControl <- function(folds = 10, repeats = 1, ...) {
  new("CVMLControl", folds = folds, repeats = repeats, ...)
}


setClass("CVMLControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl"
)


#' \code{OOBControl} constructs an MLControl object for out-of-bootstrap
#' resampling in which models are fit with bootstrap resampled training sets and
#' used to predict the unsampled cases.
#' 
#' @name OOBControl
#' @rdname MLControl-class
#' 
#' @examples
#' ## 100 out-of-bootstrap samples
#' OOBControl(samples = 100)
#' 
OOBControl <- function(samples = 25, ...) {
  new("OOBMLControl", samples = samples, ...)
}


setClass("OOBMLControl",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


#' \code{SplitControl} constructs an MLControl object for splitting data into a
#' seperate trianing and test set.
#' 
#' @param prop proportion of cases to include in the training set
#' (\code{0 < prop < 1}).
#' 
#' @name SplitControl
#' @rdname MLControl-class
#' 
#' @examples
#' ## Split sample of 2/3 training and 1/3 testing
#' SplitControl(prop = 2/3)
#' 
SplitControl <- function(prop = 2/3, ...) {
  new("SplitMLControl", prop = prop, ...)
}


setClass("SplitMLControl",
  slots = c(prop = "numeric"),
  contains = "MLControl"
)


#' \code{TrainControl} constructs an MLControl object for training and
#' performance evaluation to be performed on the same training set.
#' 
#' @name TrainControl
#' @rdname MLControl-class
#' 
#' @examples
#' ## Same training and test set
#' TrainControl()
#' 
TrainControl <- function(...) {
  new("TrainMLControl", ...)
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
#' @param name character string name for the instantiated MLModel object.
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
setClass("CForestModelFit", contains = c("MLModelFit", "RandomForest"))


#' Resamples Class Contructor
#' 
#' Create an object of resampled performance metrics from one or more models.
#' 
#' @param response data frame of resampled observed and predicted resposnes.
#' @param control MLControl object used to generate the resample output.
#' @param strata character string indicating the strata variable, if any.
#' @param ... named or unnamed resample output from one or more models.
#' 
#' @details Argument \code{control} need only be specified if the supplied
#' output is not a Resamples object.  Output being combined from more than one
#' model must have been generated with the same resampling object and
#' performance metrics.
#' 
#' @return Resamples class object.
#' 
#' @seealso \code{\link{resample}}, \code{\link{plot}}, \code{\link{summary}}
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- Species ~ .
#' control <- CVControl()
#' 
#' gbmperf1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbmperf2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbmperf3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' 
#' perf <- Resamples(GBM1 = gbmperf1, GBM2 = gbmperf2, GBM3 = gbmperf3)
#' summary(perf)
#' plot(perf)
#' 
Resamples <- function(..., control = NULL, response = NULL,
                      strata = character()) {
  new("Resamples", ..., control = control, response = response, strata = strata)
}


setClass("Resamples",
  slots = c(control = "MLControl", response = "data.frame",
            strata = "character"),
  contains = "array"
)


setMethod("initialize", "Resamples",
  function(.Object, ..., control, response, strata) {
    args <- list(...)
    
    if (length(args) == 0) stop("no resample output given")
    
    .Data <- args[[1]]
    if (length(args) == 1) {
      if (is(.Data, "Resamples")) {
        response <- .Data@response
        strata <- .Data@strata
      }
    } else {
      if (!all(sapply(args, function(x) is(x, "Resamples") && is.matrix(x)))) {
        stop("values to combine must be 2 dimensional Resamples objects")
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
      
      if (!all(sapply(args, colnames) == colnames(.Data))) {
        stop("resamples contain different metrics")
      }
      
      response <- Reduce(append, lapply(args, slot, name = "response"))
      
      old_model_names <- sapply(args, function(x) levels(x@response$Model))
      model_names <- names(args)
      if (is.null(model_names)) {
        model_names <- old_model_names
      } else {
        model_names <- ifelse(nzchar(model_names), model_names, old_model_names)
      }
      model_names <- make.names(model_names, unique = TRUE)
      num_times <- sapply(args, function(x) nrow(x@response))
      response$Model <- factor(rep(model_names, num_times),
                               levels = model_names)

      names(args) <- NULL
      args$along <- 3
      args$new.names <- list(NULL, NULL, model_names)
      .Data <- do.call(abind, args)
    }
    
    response_vars <- c("Resample", "Case", "Observed", "Predicted", "Model")
    is_missing <- !(response_vars %in% names(response))
    if (any(is_missing)) {
      stop("missing response variables: ", toString(response_vars[is_missing]))
    }
    
    callNextMethod(.Object, .Data, control = control, response = response,
                   strata = if (is.character(strata)) strata else character())
  }
)


MLModelTune <- setClass("MLModelTune",
  slots = c(grid = "data.frame", resamples = "Resamples", selected = "numeric"),
  contains = "MLModel"
)


ResamplesDiff <- setClass("ResamplesDiff",
  contains = "Resamples"
)


HTestResamples <- setClass("HTestResamples",
  slots = c("adjust" = "character"),
  contains = "array"
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
