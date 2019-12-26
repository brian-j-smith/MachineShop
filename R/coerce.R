as.data.frame.BinomialVariate <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
}


as.data.frame.ModelFrame <- function(x, ...) {
  x <- as(x, "ModelFrame")
  class(x) <- class(x)[-1]
  x <- as.data.frame(x)
  attributes(x) <- list(names = names(x), row.names = rownames(x),
                        class = "data.frame")
  x
}


setAs("ModelFrame", "data.frame",
  function(from) as.data.frame(from)
)


setAs("SelectedModelFrame", "data.frame",
  function(from) as.data.frame(from)
)


as.data.frame.ModelRecipe <- function(x, original = TRUE, ...) {
  as.data.frame(if (original) x$template else juice(prep(x)))
}


as.data.frame.PerformanceDiffTest <- function(x, ...) {
  stat_names <- matrix(NA_character_, dim(x)[1], dim(x)[2])
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


setAs("SelectedModelFrame", "ModelFrame",
  function(from) asS3(S3Part(from))
)


setAs("ModelRecipe", "recipe",
  function(from) asS3(S3Part(from))
)


setAs("SelectedRecipe", "recipe",
  function(from) asS3(S3Part(from))
)


setAs("TunedRecipe", "recipe",
  function(from) asS3(S3Part(from))
)
