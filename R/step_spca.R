#' Sparse Principal Components Analysis Variable Reduction
#'
#' Creates a \emph{specification} of a recipe step that will derive sparse
#' principal components from one or more numeric variables.
#'
#' @inheritParams step_lincomp
#' @param sparsity,num_var sparsity (L1 norm) penalty for each component or
#'   number of variables with non-zero component loadings.  Larger sparsity
#'   values produce more zero loadings.  Argument \code{sparsity} is ignored if
#'   \code{num_var} is given.  The argument value may be a single number
#'   applied to all components or a vector of component-specific numbers.
#' @param shrinkage numeric shrinkage (quadratic) penalty for the components to
#'   improve conditioning; larger values produce more shrinkage of component
#'   loadings toward zero.
#' @param center,scale logicals indicating whether to mean center and standard
#'   deviation scale the original variables prior to deriving components, or
#'   functions or names of functions for the centering and scaling.
#' @param max_iter maximum number of algorithm iterations allowed.
#' @param tol numeric tolerance for the convergence criterion.
#' @param x \code{step_spca} object.
#'
#' @return Function \code{step_spca} creates a new step whose class is of
#' the same name and inherits from \code{\link{step_lincomp}}, adds it to the
#' sequence of existing steps (if any) in the recipe, and returns the updated
#' recipe.  For the \code{tidy} method, a tibble with columns \code{terms}
#' (selectors or variables selected), \code{weight} of each variable loading in
#' the components, and \code{name} of the new variable names; and with
#' attribute \code{pev} containing the proportions of explained variation.
#'
#' @details
#' Sparse principal components analysis (SPCA) is a variant of PCA in which
#' the original variables may have zero loadings in the linear combinations
#' that form the components.
#'
#' @references
#' Zou H, Hastie T and Tibshirani R (2006). Sparse principal component analysis.
#' Journal of Computational and Graphical Statistics, 15(2):265--286.
#'
#' @seealso \code{\link[elasticnet]{spca}}, \code{\link[recipes]{recipe}},
#' \code{\link[recipes]{prep}}, \code{\link[recipes]{bake}}
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(rating ~ ., data = attitude)
#' spca_rec <- rec %>%
#'   step_spca(all_predictors(), num_comp = 5, sparsity = 1)
#' spca_prep <- prep(spca_rec, training = attitude)
#' spca_data <- bake(spca_prep, attitude)
#'
#' pairs(spca_data, lower.panel = NULL)
#'
#' tidy(spca_rec, number = 1)
#' tidy(spca_prep, number = 1)
#'
step_spca <- function(
  recipe, ..., num_comp = 5, sparsity = 0, num_var = NULL, shrinkage = 1e-6,
  center = TRUE, scale = TRUE, max_iter = 200, tol = 1e-3, replace = TRUE,
  prefix = "SPCA", role = "predictor", skip = FALSE,
  id = recipes::rand_id("spca")
) {

  recipes::add_step(recipe, new_step_spca(
    terms = recipes::ellipse_check(...),
    num_comp = num_comp,
    sparsity = sparsity,
    num_var = num_var,
    shrinkage = shrinkage,
    center = center,
    scale = scale,
    max_iter = max_iter,
    tol = tol,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  ))

}


new_step_spca <- function(..., sparsity, num_var, shrinkage, max_iter,
                          tol) {

  require_namespaces("elasticnet")

  transform <- function(x, step) {

    require_namespaces("elasticnet")

    num_comp <- min(step$num_comp, nrow(x))
    if (is.null(step$num_var)) {
      para <- step$sparsity
      sparse <- "penalty"
    } else {
      para <- step$num_var
      sparse <- "varnum"
    }
    res <- elasticnet::spca(x, K = num_comp,
                            para = rep(para, length = num_comp),
                            sparse = sparse, lambda = step$shrinkage,
                            max.iter = step$max_iter, eps.conv = step$tol)

    list(weights = res$loadings, pev = res$pev)

  }

  options <- list(
    sparsity = sparsity,
    num_var = num_var,
    shrinkage = shrinkage,
    max_iter = max_iter,
    tol = tol
  )

  object <- new_step_lincomp(..., transform = transform, options = options)

  structure(object, class = c("step_spca", class(object)))

}


#' @rdname step_spca
#'
tunable.step_spca <- function(x, ...) {
  tibble(
    name = c("num_comp", "sparsity"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 4)),
      list(pkg = "dials", fun = "penalty", range = c(-10, 0))
    ),
    source = "MachineShop",
    component = "step_spca",
    component_id = x$id
  )
}
