#' K-Means Clustering Variable Reduction
#'
#' Creates a \emph{specification} of a recipe step that will convert numeric
#' variables into one or more by averaging within k-means clusters.
#'
#' @rdname step_kmeans
#'
#' @param recipe \link[recipes]{recipe} object to which the step will be added.
#' @param ... one or more selector functions to choose which variables will be
#'   used to compute the components.  See \code{\link[recipes]{selections}} for
#'   more details.  These are not currently used by the \code{tidy} method.
#' @param k number of k-means clusterings of the variables.  The value of
#'   \code{k} is constrained to be between 1 and one less than the number of
#'   original variables.
#' @param center,scale logicals indicating whether to mean center and standard
#'   deviation scale variables prior to clustering and averaging.
#' @param algorithm character string specifying the clustering algorithm to use.
#' @param max_iter maximum number of algorithm iterations allowed.
#' @param num_start number of random cluster centers generated for starting the
#'   Hartigan-Wong algorithm.
#' @param replace logical indicating whether to replace the original variables.
#' @param prefix character string prefix added to a sequence of zero-padded
#'   integers to generate names for the resulting new variables.
#' @param role analysis role that added step variables should be assigned.  By
#'   default, they are designated as model predictors.
#' @param skip logical indicating whether to skip the step when the recipe is
#'   baked.  While all operations are baked when \code{\link[recipes]{prep}} is
#'   run, some operations may not be applicable to new data (e.g. processing
#'   outcome variables).  Care should be taken when using \code{skip = TRUE} as
#'   it may affect the computations for subsequent operations.
#' @param id unique character string to identify the step.
#' @param x \code{step_kmeans} object.
#'
#' @return An updated version of \code{recipe} with the new step added to the
#' sequence of existing steps (if any).  For the \code{tidy} method, a tibble
#' with columns \code{terms} (selectors or variables selected), \code{cluster}
#' assignments, \code{sqdist} (squared distance from cluster centers), and
#' \code{names} of the new variables.
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

  recipes::add_step(recipe, recipes::step(
    subclass = "kmeans",
    terms = recipes::ellipse_check(...),
    k = k,
    center = if (isTRUE(center)) base::mean else FALSE,
    scale = if (isTRUE(scale)) stats::sd else FALSE,
    algorithm = match.arg(algorithm),
    max_iter = max_iter,
    num_start = num_start,
    res = list(),
    trained = FALSE,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  ))

}


prep.step_kmeans <- function(x, training, info = NULL, ...) {

  col_names <- recipes::terms_select(terms = x$terms, info = info)
  training <- training[col_names]
  recipes::check_type(training)

  x$k <- max(min(x$k, length(col_names) - 1), 1)

  if (is.function(x$center)) x$center <- apply(training, 2, x$center)
  if (is.function(x$scale)) x$scale <- apply(training, 2, x$scale)
  training <- scale(training, center = x$center, scale = x$scale)

  res <- stats::kmeans(t(training), centers = x$k, iter.max = x$max_iter,
                       nstart = x$num_start, algorithm = x$algorithm)
  names(res$size) <- recipes::names0(length(res$size), x$prefix)

  res$sqdist <- numeric(length(res$cluster))
  for (i in unique(res$cluster)) {
    in_cluster <- res$cluster == i
    res$sqdist[in_cluster] <-
      colSums((training[, in_cluster, drop = FALSE] - res$centers[i, ])^2)
  }

  x$res <- res[c("cluster", "size", "sqdist")]
  x$trained <- TRUE
  x

}


bake.step_kmeans <- function(object, new_data, ...) {

  res <- object$res

  is_cluster_var <- names(new_data) %in% names(res$cluster)

  nz <- res$size > 0
  cluster_data <- scale(new_data[is_cluster_var],
                        center = object$center, scale = object$scale)
  cluster_weights <- Matrix::t(Matrix::fac2sparse(res$cluster) / res$size[nz])
  cluster_means <- as_tibble(as.matrix(cluster_data %*% cluster_weights))

  if (object$replace) new_data <- new_data[!is_cluster_var]
  cluster_means <- recipes::check_name(cluster_means, new_data, object,
                                       newname = names(res$size)[nz])

  as_tibble(c(new_data, cluster_means))

}


print.step_kmeans <- function(x, width = max(20, options()$width - 31), ...) {
  cat("K-means cluster extraction for ")
  recipes::printer(names(x$res$cluster), x$terms, x$trained, width = width)
  invisible(x)
}


#' @rdname step_kmeans
#'
tidy.step_kmeans <- function(x, ...) {
  res <- if (is.trained(x)) {
    cluster <- x$res$cluster
    tibble(
      terms = names(cluster),
      cluster = cluster,
      sqdist = x$res$sqdist,
      names = names(x$res$size)[cluster]
    )
  } else {
    tibble(
      terms = recipes::sel2char(x$terms),
      cluster = NA_integer_,
      sqdist = NA_real_,
      names = NA_character_
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
