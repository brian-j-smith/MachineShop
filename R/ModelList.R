#' Model List
#'
#' Create a list of models.
#' 
#' @aliases MLModelList
#' 
#' @param ... \link[=models]{model} functions, function names, calls, or vectors
#' of these.
#' 
#' @return \code{MLModelList} class object that inherits from \code{list}.
#' 
#' @seealso \code{\link{tune}}, \code{\link{SelectedModel}}
#' 
#' @examples
#' ModelList(GBMModel, GLMNetModel, RandomForestModel)
#' 
ModelList <- function(...) {
  models <- as.list(unlist(list(...)))
  model_names <- character()
  for (i in seq(models)) {
    models[[i]] <- getMLObject(models[[i]], class = "MLModel")
    name <- names(models)[i]
    model_names[i] <- 
      if (!is.null(name) && nzchar(name)) name else models[[i]]@name
  }
  names(models) <- make.unique(model_names)
  MLModelList(models)
}
