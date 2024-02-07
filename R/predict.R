#' Model Prediction
#'
#' Predict outcomes with a fitted model.
#'
#' @name predict
#'
#' @param object model \link{fit} result.
#' @param newdata optional \link[=data.frame]{data frame} with which to obtain
#'   predictions.  If not specified, the training data will be used by default.
#' @param times numeric vector of follow-up times at which to predict
#'   survival events/probabilities or \code{NULL} for predicted survival means.
#' @param type specifies prediction on the original outcome (\code{"response"}),
#'   numeric (\code{"numeric"}), or probability (\code{"prob"}) scale; or
#'   the \code{"raw"} predictions returned by the model.  Option
#'   \code{"default"} is deprecated and will be removed in the future; use
#'   \code{"raw"} instead.
#' @param cutoff numeric (0, 1) threshold above which binary factor
#'   probabilities are classified as events and below which survival
#'   probabilities are classified.
#' @param distr character string specifying distributional approximations to
#'   estimated survival curves.  Possible values are \code{"empirical"},
#'   \code{"exponential"}, \code{"rayleigh"}, or \code{"weibull"}; with defaults
#'   of \code{"empirical"} for predicted survival events/probabilities and
#'   \code{"weibull"} for predicted survival means.
#' @param method character string specifying the empirical method of estimating
#'   baseline survival curves for Cox proportional hazards-based models.
#'   Choices are \code{"breslow"} or \code{"efron"} (default).
#' @param verbose logical indicating whether to display printed output generated
#'   by some model-specific predict functions to aid in monitoring progress and
#'   diagnosing errors.
#' @param ... arguments passed from the S4 to the S3 method.
#'
#' @seealso \code{\link{confusion}}, \code{\link{performance}},
#' \code{\link{metrics}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Survival response example
#' library(survival)
#'
#' gbm_fit <- fit(Surv(time, status) ~ ., data = veteran, model = GBMModel)
#' predict(gbm_fit, newdata = veteran, times = c(90, 180, 360), type = "prob")
#' }
#'
NULL


#' @name predict
#' @method predict MLModelFit
#'
predict.MLModelFit <- function(
  object, newdata = NULL, times = numeric(),
  type = c("response", "raw", "numeric", "prob", "default"),
  cutoff = MachineShop::settings("cutoff"), distr = character(),
  method = character(), verbose = FALSE, ...
) {
  object <- update(object)
  model <- as.MLModel(object)
  throw(check_packages(model@packages))

  newdata <- PredictorFrame(as.MLInput(object), newdata)
  newnames <- rownames(newdata)
  newdata <- ModelFrame(newdata, na.rm = model@na.rm)
  if (!nrow(newdata)) throw(Error("No case observations to predict."))
  subset <- newnames %in% rownames(newdata)

  times <- check_numeric(
    times, bounds = c(0, Inf), include = FALSE, size = NA, nonempty = FALSE
  )
  throw(check_assignment(times))

  type <- match.arg(type)
  if (type == "default") {
    throw(DeprecatedCondition("type = \"default\"", "type = \"raw\""))
    type <- "raw"
  }

  .MachineShop <- attr(object, ".MachineShop")
  (if (verbose) identity else capture.output)(
    pred <- .predict(
      model, model_fit = unMLModelFit(object), newdata = newdata, times = times,
      distr = distr, method = method, .MachineShop = .MachineShop, ...
    )
  )
  obs <- response(object)
  pred <- convert_predicted(obs, pred)
  pred <- switch(type,
    "raw" = pred,
    "numeric" = convert_numeric(pred),
    "prob" = convert_numeric(pred, bounds = c(0, 1)),
    "response" = convert_response(obs, pred, cutoff = cutoff)
  )
  throw(check_assignment(pred))
  na.add(pred, !subset)
}


#' @name predict
#' @aliases predict,MLModelFit-method
#'
setMethod("predict", "MLModelFit",
  function(object, ...) predict.MLModelFit(object, ...)
)


.predict <- function(object, ...) {
  UseMethod(".predict")
}


.predict.MLModel <- function(object, model_fit, ...) {
  object@predict(model_fit, ...)
}


PredictorFrame <- function(input, newdata = NULL) {
  if (!is(newdata, "PredictorFrame")) {
    new("PredictorFrame", ModelFrame(
      delete.response(terms(input)), predictors(input, newdata), na.rm = FALSE
    ))
  } else {
    newdata
  }
}


predictors <- function(object, ...) {
  UseMethod("predictors")
}


predictors.formula <- function(object, data = NULL, ...) {
  if (is.null(data)) {
    object[[length(object)]]
  } else {
    data[, all.vars(predictors(object)), drop = FALSE]
  }
}


predictors.ModelFrame <- function(object, newdata = NULL, ...) {
  data <- as.data.frame(if (is.null(newdata)) object else newdata)
  predictors(terms(object), data)
}


predictors.recipe <- function(object, newdata = NULL, ...) {
  object <- prep(object, retain = FALSE)
  data <- bake(object, newdata = newdata)
  info <- summary(object)
  pred_names <- info$variable[info$role %in% c("predictor", "pred_offset")]
  data[, pred_names, drop = FALSE]
}
