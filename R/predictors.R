#' Model Formula Offset
#' 
#' An offset is a term to be added to a linear predictor, such as in a
#' generalized linear model, with known coefficient 1 rather than an estimated
#' coefficient.
#' 
#' @param object offset term to include in a model formula.
#' 
#' @seealso \code{\link[stats:offset]{offset}}
#' 
offset <- stats::offset


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
  data <- if (is.null(newdata)) juice(object) else bake(object, newdata)
  info <- summary(object)
  data[, info$variable[info$role %in% c("predictor", "offset")], drop = FALSE]
}
