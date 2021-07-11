setAsS3Part <- function(from, to) {
  setAs(from, to, function(from) {
    if (!isS4(from)) throw(TypeError(from, "S4 class"))
    asS3(S3Part(from))
  })
}


as.data.frame.BinomialVariate <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
}


setAsS3Part("ListOf", "listof")


as.data.frame.ModelFrame <- function(x, ...) {
  structure(as(x, "ModelFrame"), terms = NULL, class = "data.frame")
}


setAs("ModelFrame", "data.frame",
  function(from) as.data.frame(from)
)


setAs("ModeledFrame", "data.frame",
  function(from) as.data.frame(from)
)


setAsS3Part("ParameterGrid", "parameters")


setAs("recipe", "ModelRecipe",
  function(from) ModelRecipe(from)
)


setAsS3Part("RecipeGrid", "tbl_df")


setAs("SelectedModelFrame", "data.frame",
  function(from) as.data.frame(from)
)


as.data.frame.ModelRecipe <- function(x, original = TRUE, ...) {
  as.data.frame(if (original) x$template else juice(prep(x)))
}


as.data.frame.PerformanceDiffTest <- function(x, ...) {
  stat_names <- matrix(NA_character_, nrow(x), ncol(x))
  stat_names[upper.tri(stat_names)] <- "Mean"
  stat_names[lower.tri(stat_names)] <- "P-Value"
  df_stat_names <- as.data.frame(TabularArray(stat_names))

  x <- cbind(NextMethod(), Statistic = df_stat_names$Value)
  x <- x[!is.na(x$Statistic), ]
  is_pval <- x$Statistic == "P-Value"
  x[is_pval, c("Model1", "Model2")] <- x[is_pval, c("Model2", "Model1")]
  x$Model <- paste(x$Model1, "-", x$Model2)

  ind <- order(x$Statistic, x$Metric, x$Model)
  x <- x[ind, c("Statistic", "Metric", "Model", "Value")]
  rownames(x) <- NULL
  x
}


as.data.frame.Resamples <- function(x, ...) {
  asS3(as(x, "data.frame"))
}


as.data.frame.SurvMatrix <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
}


as.data.frame.TabularArray <- function(x, ..., responseName = "Value") {
  as.data.frame.table(as(x, "array"), responseName = responseName, ...)
}


as.double.BinomialVariate <- function(x, ...) {
  as.numeric(x[, "Success"] / (x[, "Success"] + x[, "Failure"]))
}


#' Coerce to an MLModel
#'
#' Function to coerce an \code{MLModelFit} object to an \code{MLModel}.
#'
#' @rdname as.MLModel
#'
#' @param x model \link{fit} result.
#' @param ... arguments passed to other methods.
#'
#' @return \code{MLModel} class object.
#'
as.MLModel <- function(x, ...) {
  UseMethod("as.MLModel")
}


#' @rdname as.MLModel
#'
as.MLModel.MLModelFit <- function(x, ...) {
  getElement(x, "mlmodel")
}


setAsS3Part("ModeledFrame", "ModelFrame")


setAsS3Part("SelectedModelFrame", "ModelFrame")


setAsS3Part("ModelRecipe", "recipe")


setAsS3Part("ModeledRecipe", "recipe")


setAsS3Part("SelectedModelRecipe", "recipe")


setAsS3Part("TunedModelRecipe", "recipe")
