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
  object <- prep(object)
  data <- bake(object, newdata)
  info <- summary(object)
  vars <- info$variable[info$role %in% c("predictor", "pred_offset")]
  data[, vars, drop = FALSE]
}


predictor_frame <- function(model, newdata = NULL) {
  stopifnot(is(model, "MLModel") && !is.null(model@x))
  x <- model@x
  ModelFrame(delete.response(terms(x)), predictors(x, newdata), na.rm = FALSE)
}
