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
  data <- bake(object, newdata = newdata)
  info <- summary(object)
  pred_names <- info$variable[info$role %in% c("predictor", "pred_offset")]
  data[, pred_names, drop = FALSE]
}


predictor_frame <- function(model, newdata = NULL) {
  stopifnot(is(model, "MLModel") && !is(model@input, "NullInput"))
  input <- model@input
  ModelFrame(delete.response(terms(input)), predictors(input, newdata),
             na.rm = FALSE)
}
