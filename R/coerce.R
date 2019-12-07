as.data.frame.BinomialVariate <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
}


as.data.frame.ModelFrame <- function(x, ...) {
  x <- NextMethod()
  attributes(x) <- list(names = names(x), row.names = rownames(x),
                        class = "data.frame")
  x
}


setAs("ModelFrame", "data.frame",
  function(from) as.data.frame(from)
)


as.data.frame.ModelRecipe <- function(x, original = TRUE, ...) {
  as.data.frame(if (original) x$template else juice(prep(x)))
}


as.data.frame.SelectedModelFrame <- function(x, ...) {
  as.data.frame(as(x, "ModelFrame"))
}


as.data.frame.SurvMatrix <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
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
  function(from) asS3(from)
)


setAs("ModelRecipe", "recipe",
  function(from) asS3(from)
)


setAs("TunedRecipe", "recipe",
  function(from) asS3(from)
)
