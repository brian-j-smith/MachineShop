#' K-Means Clustering Variable Reduction
#'
#' Creates a \emph{specification} of a recipe step that will convert numeric
#' variables into one or more by averaging within k-means clusters.
#'
#' @inheritParams step_lincomp
#' @param k number of k-means clusterings of the variables.  The value of
#'   \code{k} is constrained to be between 1 and one less than the number of
#'   original variables.
#' @param center,scale logicals indicating whether to mean center and standard
#'   deviation scale the original variables prior to deriving components, or
#'   functions or names of functions for the centering and scaling.
#' @param algorithm character string specifying the clustering algorithm to use.
#' @param max_iter maximum number of algorithm iterations allowed.
#' @param num_start number of random cluster centers generated for starting the
#'   Hartigan-Wong algorithm.
#' @param x \code{step_kmeans} object.
#'
#' @return Function \code{step_kmeans} creates a new step whose class is of
#' the same name and inherits from \code{\link{step_lincomp}}, adds it to the
#' sequence of existing steps (if any) in the recipe, and returns the updated
#' recipe.  For the \code{tidy} method, a tibble with columns \code{terms}
#' (selectors or variables selected), \code{cluster} assignments, \code{sqdist}
#' (squared distance from cluster centers), and \code{name} of the new variable
#' names.
#'
#' @details
#' K-means clustering partitions variables into k groups such that the sum of
#' squares between the variables and their assigned cluster means is minimized.
#' Variables within each cluster are then averaged to derive a new set of k
#' variables.
#'
#' @references
#' Forgy EW (1965). Cluster analysis of multivariate data: efficiency vs
#' interpretability of classifications. Biometrics 21, 768--769.
#'
#' Hartigan JA and Wong MA (1979). A K-means clustering algorithm.  Applied
#' Statistics 28, 100--108.
#'
#' Lloyd SP (1957, 1982). Least squares quantization in PCM. Technical Note,
#' Bell Laboratories. Published in 1982 in IEEE Transactions on Information
#' Theory 28, 128--137.
#'
#' MacQueen J (1967). Some methods for classification and analysis of
#' multivariate observations. In Proceedings of the Fifth Berkeley Symposium
#' on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman,
#' 1, 281--297. Berkeley, CA: University of California Press.
#'
#' @seealso \code{\link[stats]{kmeans}}, \code{\link[recipes]{recipe}},
#' \code{\link[recipes]{prep}}, \code{\link[recipes]{bake}}
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(rating ~ ., data = attitude)
#' kmeans_rec <- rec %>%
#'   step_kmeans(all_predictors(), k = 3)
#' kmeans_prep <- prep(kmeans_rec, training = attitude)
#' kmeans_data <- bake(kmeans_prep, attitude)
#'
#' pairs(kmeans_data, lower.panel = NULL)
#'
#' tidy(kmeans_rec, number = 1)
#' tidy(kmeans_prep, number = 1)
#'
step_kmeans <- function(recipe, ..., k = 5, center = TRUE, scale = TRUE,
                        algorithm =
                          c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                        max_iter = 10, num_start = 1, replace = TRUE,
                        prefix = "KMeans", role = "predictor", skip = FALSE,
                        id = recipes::rand_id("kmeans")) {

  recipes::add_step(recipe, new_step_kmeans(
    terms = recipes::ellipse_check(...),
    k = k,
    center = center,
    scale = scale,
    algorithm = match.arg(algorithm),
    max_iter = max_iter,
    num_start = num_start,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  ))

}


new_step_kmeans <- function(..., k, algorithm, max_iter, num_start) {

  transform <- function(x, step) {

    res <- stats::kmeans(t(x), centers = max(min(step$k, ncol(x) - 1), 1),
                         iter.max = step$max_iter, nstart = step$num_start,
                         algorithm = step$algorithm)

    cluster <- res$cluster
    weights <- Matrix::t(Matrix::fac2sparse(cluster) / res$size[res$size > 0])
    sqdist <- numeric(length(cluster))
    for (i in unique(cluster)) {
      in_cluster <- cluster == i
      x_centered <- x[, in_cluster, drop = FALSE] - res$centers[i, ]
      sqdist[in_cluster] <- colSums(x_centered^2)
    }

    list(weights = weights, cluster = cluster, sqdist = sqdist)

  }

  options <- list(
    k = k,
    algorithm = algorithm,
    max_iter = max_iter,
    num_start = num_start
  )

  object <- new_step_lincomp(..., transform = transform, num_comp = NULL,
                             options = options)
  object$res <- tibble(
    terms = recipes::sel2char(object$terms),
    cluster = NA_integer_,
    sqdist = NA_real_,
    name = NA_character_
  )

  structure(object, class = c("step_kmeans", class(object)))

}


#' @rdname step_kmeans
#'
tidy.step_kmeans <- function(x, ...) {
  res <- x$res
  if (is.trained(x)) {
    cluster <- x$res$cluster
    res <- tibble(
      terms = rownames(x$res$weights),
      cluster = cluster,
      sqdist = x$res$sqdist,
      name = colnames(x$res$weights)[cluster]
    )
  }
  res$id <- x$id
  res
}


#' @rdname step_kmeans
#'
tunable.step_kmeans <- function(x, ...) {
  tibble(
    name = "k",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(2, 10))),
    source = "MachineShop",
    component = "step_kmeans",
    component_id = x$id
  )
}
