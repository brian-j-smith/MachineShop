#' Model Expansion Over Tuning Parameters
#'
#' Expand a model over all combinations of a grid of tuning parameters.
#'
#' @param x \link[=models]{model} function, function name, or call.
#' @param ... named vectors or factors or a list of these containing the
#'   parameter values over which to expand \code{x}.
#' @param random number of points to be randomly sampled from the parameter grid
#'   or \code{FALSE} if all points are to be returned.
#'
#' @return \code{list} of expanded models.
#'
#' @seealso \code{\link{SelectedModel}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' data(Boston, package = "MASS")
#'
#' models <- expand_model(GBMModel, n.trees = c(50, 100),
#'                                  interaction.depth = 1:2)
#'
#' fit(medv ~ ., data = Boston, model = SelectedModel(models))
#' }
#'
expand_model <- function(x, ..., random = FALSE) {
  .expand_model(x, random, ...)
}


.expand_model <- function(x, ...) {
  UseMethod(".expand_model")
}


.expand_model.default <- function(x, random, ...) {
  expand_model(getMLObject(x, class = "MLModel"), ..., random = random)
}


.expand_model.list <- function(x, ...) {
  grid <- x[[2]]
  models <- map(function(args) do.call(x[[1]], args),
                split(grid, seq(max(1, nrow(grid)))))
  names(models) <- paste0(models[[1]]@name, ".", names(models))
  models
}


.expand_model.MLModel <- function(x, random, ...) {
  grid <- expand_params(..., random = random)
  expand_model(list(fget(x@name), grid))
}


#' Model Parameters Expansion
#'
#' Create a grid of parameter values from all combinations of supplied inputs.
#'
#' @param ... named vectors or factors or a list of these containing the
#'   parameter values over which to create the grid.
#' @param random number of points to be randomly sampled from the parameter grid
#'   or \code{FALSE} if all points are to be returned.
#'
#' @return A data frame containing one row for each combination of the supplied
#' inputs.
#'
#' @seealso \code{\link{TunedModel}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' data(Boston, package = "MASS")
#'
#' grid <- expand_params(
#'   n.trees = c(50, 100),
#'   interaction.depth = 1:2
#' )
#'
#' fit(medv ~ ., data = Boston, model = TunedModel(GBMModel, grid = grid))
#' }
#'
expand_params <- function(..., random = FALSE) {
  if (random) {
    x <- list(...)
    if (is_one_element(x, "list") && is.null(names(x))) x <- x[[1]]
    sample_params(x, size = random)
  } else {
    as_tibble(expand.grid(..., KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE))
  }
}


#' Recipe Step Parameters Expansion
#'
#' Create a grid of parameter values from all combinations of lists supplied for
#' steps of a preprocessing recipe.
#'
#' @param ... one or more lists containing parameter values over which to create
#'   the grid.  For each list an argument name should be given as the \code{id}
#'   of the \link[recipes]{recipe} step to which it corresponds.
#' @param random number of points to be randomly sampled from the parameter grid
#'   or \code{FALSE} if all points are to be returned.
#'
#' @return \code{RecipeGrid} class object that inherits from \code{data.frame}.
#'
#' @seealso \code{\link{TunedInput}}
#'
#' @examples
#' library(recipes)
#' data(Boston, package = "MASS")
#'
#' rec <- recipe(medv ~ ., data = Boston) %>%
#'   step_corr(all_numeric(), -all_outcomes(), id = "corr") %>%
#'   step_pca(all_numeric(), -all_outcomes(), id = "pca")
#'
#' expand_steps(
#'   corr = list(threshold = c(0.8, 0.9),
#'               method = c("pearson", "spearman")),
#'   pca = list(num_comp = 1:3)
#' )
#'
expand_steps <- function(..., random = FALSE) {

  steps <- list(...)
  step_names <- names(steps)
  if (is_one_element(steps, "list") && is.null(step_names)) {
    steps <- steps[[1]]
    step_names <- names(steps)
  }

  if (!all(map_logi(is.list, steps))) stop("step arguments must be lists")

  get_names <- function(x) {
    res <- NULL
    if (is.list(x)) {
      for (i in seq(x)) {
        name <- names(x[i])
        if (is.null(name)) name <- ""
        res <- c(res, name, get_names(x[[i]]))
      }
    }
    res
  }

  if (!all(nzchar(get_names(steps)))) {
    stop("all steps and their parameters must be named")
  } else if (any(duplicated(step_names))) {
    stop("step names must be unique")
  }

  grid <- expand_params(unlist(steps, recursive = FALSE), random = random)
  recipe_grid <- tibble(.rows = nrow(grid))

  offset <- 0
  for (name in step_names) {
    indices <- offset + seq_len(length(steps[[name]]))
    x <- grid[indices]
    names(x) <- substring(names(x), nchar(name) + 2)
    recipe_grid[[name]] <- x
    offset <- offset + length(indices)
  }

  RecipeGrid(recipe_grid)

}
