#' Sparse Principal Components Analysis Variable Reduction
#'
#' Creates a \emph{specification} of a recipe step that will derive sparse
#' principal components from one or more numeric variables.
#'
#' @rdname step_spca
#'
#' @inheritParams step_kmeans
#' @param num_comp number of principal components to derive.  The value of
#'   \code{num_comp} is constrained to be between 1 and the number of original
#'   variables.
#' @param sparsity,num_var sparsity (L1 norm) penalty for each component or
#'   number of variables with non-zero component loadings.  Larger sparsity
#'   values produce more zero loadings.  Argument \code{sparsity} is ignored if
#'   \code{num_var} is given.  The argument value may be a single number
#'   applied to all components or a vector of component-specific numbers.
#' @param shrinkage numeric shrinkage (quadratic) penalty for the components to
#'   improve conditioning; larger values produce more shrinkage of component
#'   loadings toward zero.
#' @param center,scale logicals indicating whether to mean center and standard
#'   deviation scale variables prior to deriving components.
#' @param tol numeric tolerance for the convergence criterion.
#' @param x \code{step_spca} object.
#'
#' @return An updated version of \code{recipe} with the new step added to the
#' sequence of existing steps (if any).  For the \code{tidy} method, a tibble
#' with columns \code{terms} (selectors or variables selected), loading
#' \code{value}, and \code{component} names of the new variables; and with
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
step_spca <- function(recipe, ..., num_comp = 5, sparsity = 0, num_var = NULL,
                      shrinkage = 1e-6, center = TRUE, scale = TRUE,
                      max_iter = 200, tol = 1e-3, replace = TRUE,
                      prefix = "SPCA", role = "predictor", skip = FALSE,
                      id = recipes::rand_id("spca")) {

  requireModelNamespaces("elasticnet")

  recipes::add_step(recipe, recipes::step(
    subclass = "spca",
    terms = recipes::ellipse_check(...),
    num_comp = num_comp,
    sparsity = if (is.null(num_var)) sparsity,
    num_var = num_var,
    shrinkage = shrinkage,
    center = if (isTRUE(center)) base::mean else FALSE,
    scale = if (isTRUE(scale)) stats::sd else FALSE,
    max_iter = max_iter,
    tol = tol,
    res = list(pev = NA_real_),
    trained = FALSE,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  ))

}


prep.step_spca <- function(x, training, info = NULL, ...) {

  requireModelNamespaces("elasticnet")

  col_names <- recipes::terms_select(terms = x$terms, info = info)
  training <- training[col_names]
  recipes::check_type(training)

  x$num_comp <- max(min(x$num_comp, length(col_names)), 1)
  if (is.null(x$num_var)) {
    x$sparsity <- rep(x$sparsity, length.out = x$num_comp)
    para <- x$sparsity
    sparse <- "penalty"
  } else {
    x$num_var <- rep(x$num_var, length.out = x$num_comp)
    para <- x$num_var
    sparse <- "varnum"
  }

  if (is.function(x$center)) x$center <- apply(training, 2, x$center)
  if (is.function(x$scale)) x$scale <- apply(training, 2, x$scale)
  training <- scale(training, center = x$center, scale = x$scale)

  res <- elasticnet::spca(training, K = x$num_comp, para = para,
                          sparse = sparse, lambda = x$shrinkage,
                          max.iter = x$max_iter, eps.conv = x$tol)
  dimnames(res$loadings) <- list(
    terms = colnames(training),
    component = recipes::names0(ncol(res$loadings), x$prefix)
  )
  names(res$pev) <- colnames(res$loadings)

  x$res <- res[c("loadings", "pev")]
  x$trained <- TRUE
  x

}


bake.step_spca <- function(object, new_data, ...) {

  loadings <- object$res$loadings
  is_spca_var <- names(new_data) %in% rownames(loadings)

  spca_data <- scale(new_data[is_spca_var],
                     center = object$center, scale = object$scale)
  pc_data <- as_tibble(spca_data %*% loadings)
  names(pc_data) <- colnames(loadings)

  if (object$replace) new_data <- new_data[!is_spca_var]
  pc_data <- recipes::check_name(pc_data, new_data, object)

  as_tibble(c(new_data, pc_data))

}


print.step_spca <- function(x, width = max(20, options()$width - 26), ...) {
  cat("Sparse PCA extraction for ")
  recipes::printer(rownames(x$res$loadings), x$terms, x$trained, width = width)
  invisible(x)
}


#' @rdname step_spca
#'
tidy.step_spca <- function(x, ...) {
  if (is.trained(x)) {
    res <- as.data.frame(as.table(x$res$loadings),
                         responseName = "value",
                         stringsAsFactors = FALSE)
    res <- as_tibble(res[c("terms", "value", "component")])
  } else {
    res <- tibble(
      terms = recipes::sel2char(x$terms),
      value = NA_real_,
      component = NA_character_
    )
  }
  res$id <- x$id
  attr(res, "pev") <- x$res$pev
  res
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
