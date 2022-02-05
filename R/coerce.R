setAsS3Part <- function(from, to) {
  setAs(from, to, function(from) {
    if (!isS4(from)) throw(TypeError(from, "S4 class"))
    asS3(S3Part(from))
  })
}


as.data.frame.BinomialVariate <- function(x, ...) {
  as.data.frame.model.matrix(x, ...)
}


as.data.frame.formula <- function(x, data, ...) {
  eval.parent(substitute(environment(x) <- environment()))
  as.data.frame(data, ...)
}


as.data.frame.ModelFrame <- function(x, ...) {
  structure(asS3(S3Part(x)), terms = NULL)
}


setAs("ModelFrame", "data.frame",
  function(from) as.data.frame(from)
)


setAs("ModeledFrame", "data.frame",
  function(from) as.data.frame(from)
)


setAs("SelectedModelFrame", "data.frame",
  function(from) as.data.frame(from)
)


as.data.frame.ModelRecipe <- function(x, ...) {
  as.data.frame(x$template)
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


as.data.frame.Resample <- function(x, ...) {
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


setAs("MLModel", "list",
  function(from) as(from@params, "list")
)


setAs("SelectedInput", "list",
  function(from) c(list(objects = from@inputs), as(from@params, "list"))
)


setAs("SelectedModel", "list",
  function(from) c(list(objects = from@models), as(from@params, "list"))
)


setAs("StackedModel", "list",
  function(from) {
    c(list(
      objects = from@models,
      control = from@params@control),
      from@params@options
    )
  }
)


setAs("SuperModel", "list",
  function(from) {
    res <- callNextMethod()
    res$model <- res$objects[[1]]
    res$objects[1] <- NULL
    res
  }
)


setAs("TrainingParams", "list",
  function(from) {
    res <- map(function(name) slot(from, name), slotNames(from))
    options <- res$options
    res$options <- NULL
    c(new_params(res), options)
  }
)


setAsS3Part("ListOf", "listof")


as.MLControl <- function(x, ...) {
  UseMethod("as.MLControl")
}


as.MLControl.default <- function(x, ...) {
  throw(Error("Cannot coerce class ", class1(x), " to an MLControl."))
}


as.MLControl.character <- function(x, ...) {
  x <- get0(x)
  as.MLControl(x)
}


as.MLControl.function <- function(x, ...) {
  result <- try(x(), silent = TRUE)
  if (!is(result, "MLControl")) {
    throw(Error("Cannot coerce specified function to an MLControl"))
  } else result
}


as.MLControl.MLControl <- function(x, ...) {
  x
}


as.MLInput <- function(x, ...) {
  UseMethod("as.MLInput")
}


as.MLInput.default <- function(x, ...) {
  throw(Error("Cannot coerce class ", class1(x), " to an MLInput."))
}


as.MLInput.formula <- function(x, data, ...) {
  args <- list(x, data, strata = response(x), na.rm = FALSE)
  do.call(ModelFrame, args)
}


as.MLInput.matrix <- function(x, y, ...) {
  ModelFrame(x, y, strata = y, na.rm = FALSE)
}


as.MLInput.MLInput <- function(x, ...) {
  x
}


as.MLInput.recipe <- function(x, ...) {
  ModelRecipe(x)
}


as.MLMetric <- function(x, ...) {
  UseMethod("as.MLMetric")
}


as.MLMetric.default <- function(x, ...) {
  throw(Error("Cannot coerce class ", class1(x), " to an MLMetric."))
}


as.MLMetric.character <- function(x, ...) {
  x <- get0(x)
  as.MLMetric(x)
}


as.MLMetric.MLMetric <- function(x, ...) {
  x
}


#' Coerce to an MLModel
#'
#' Function to coerce an object to \code{MLModel}.
#'
#' @param x model \link{fit} result, \link[=ModeledInput]{modeled input}, or
#'   \link[parsnip:model_spec]{model specification} from the \pkg{parsnip}
#'   package.
#' @param ... arguments passed to other methods.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link{ParsnipModel}}
#'
as.MLModel <- function(x, ...) {
  UseMethod("as.MLModel")
}


as.MLModel.default <- function(x, ...) {
  throw(Error("Cannot coerce class ", class1(x), " to an MLModel."))
}


as.MLModel.character <- function(x, ...) {
  x <- get0(x)
  as.MLModel(x)
}


as.MLModel.MLModel <- function(x, ...) {
  x
}


#' @rdname as.MLModel
#'
as.MLModel.MLModelFit <- function(x, ...) {
  getElement(x, "mlmodel")
}


as.MLModel.MLModelFunction <- function(x, ...) {
  x()
}


#' @rdname as.MLModel
#'
as.MLModel.ModeledInput <- function(x, ...) {
  x@model
}


#' @rdname as.MLModel
#'
as.MLModel.model_spec <- function(x, ...) {
  ParsnipModel(x)
}


as.MLModel.NULL <- function(x, ...) {
  NullModel()
}


setAs("recipe", "ModelRecipe",
  function(from) ModelRecipe(from)
)


setAsS3Part("ParameterGrid", "parameters")


setAsS3Part("ModelRecipe", "recipe")


setAsS3Part("ModeledRecipe", "recipe")


setAsS3Part("SelectedModelRecipe", "recipe")


setAsS3Part("TunedModelRecipe", "recipe")


setAsS3Part("RecipeGrid", "tbl_df")
