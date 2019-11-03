#' Selected Recipe
#' 
#' Recipe selection from a candidate set.
#' 
#' @param ... untrained \code{\link[recipes:recipe]{recipes}} or a list of them.
#' @param control \link[=controls]{control} function, function name, or call
#'   defining the resampling method to be employed.
#' @param metrics \link[=metrics]{metric} function, function name, or vector of
#'   these with which to calculate performance.  If not specified, default
#'   metrics defined in the \link{performance} functions are used.  Recipe
#'   selection is based on the first calculated metric.
#' @param stat function or character string naming a function to compute a
#'   summary statistic on resampled metric values for recipe tuning.
#' @param cutoff argument passed to the \code{metrics} functions.
#' 
#' @return \code{SelectedRecipe} class object that inherits from \code{recipe}.
#' 
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#' 
#' @examples
#' library(recipes)
#' library(MASS)
#' 
#' rec1 <- recipe(medv ~ crim + zn + indus + chas + nox + rm, data = Boston)
#' rec2 <- recipe(medv ~ chas + nox + rm + age + dis + rad + tax, data = Boston)
#' 
#' fit(SelectedRecipe(rec1, rec2), model = GLMModel)
#' 
SelectedRecipe <- function(..., control = MachineShop::settings("control"),
                           metrics = NULL,
                           stat = MachineShop::settings("stat.Tune"),
                           cutoff = MachineShop::settings("cutoff")) {
  
  recipes <- list(...)
  if (length(recipes) == 1 && is(recipes, "list") && !is(recipes, "recipe")) {
    recipes <- recipes[[1]]
  }
  recipe_names <- names(recipes)
  names(recipes) <- paste0("Recipe.", seq(recipes))
  if (!is.null(recipe_names)) {
    keep <- nzchar(recipe_names)
    names(recipes)[keep] <- recipe_names[keep]
  }

  for (i in seq(recipes)) recipes[[i]] <- ModelRecipe(recipes[[i]])
  
  getinfo <- function(x) {
    info <- summary(x)
    info <- info[info$role != "predictor", ]
    info_order <- do.call(order, info)
    info[info_order, ]
  }
  
  info <- getinfo(recipes[[1]])
  same_info <- sapply(recipes[-1], function(x) {
    identical(info, getinfo(x))
  })
  
  if (!all(same_info)) stop("non-predictor variables in recipes differ")
  
  combine <- function(x, y) {
    if (is.null(x)) x <- data.frame(row.names = seq_len(nrow(y)))
    common_vars <- intersect(names(x), names(y))
    if (!identical(x[common_vars], y[common_vars])) {
      stop("common variables in recipes differ")
    }
    diff_vars <- setdiff(names(y), common_vars)
    x[diff_vars] <- y[diff_vars]
    x
  }
  
  data <- NULL
  for (i in seq(recipes)) {
    data <- combine(data, as.data.frame(recipes[[i]]))
    recipes[[i]] <- recipe(recipes[[i]], tibble())
  }
  
  outcome_vars <- info$variable[info$role == "outcome"]
  fo <- reformulate(".", paste(outcome_vars, collapse = "+"))
  new("SelectedRecipe", new("ModelRecipe", recipe(fo, data = data)),
      recipes = recipes,
      params = list(control = getMLObject(control, "MLControl"),
                    metrics = metrics, stat = stat, cutoff = cutoff))

}
