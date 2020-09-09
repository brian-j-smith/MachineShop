#' K-Medoids Clustering Variable Selection
#'
#' Creates a \emph{specification} of a recipe step that will partition numeric
#' variables according to k-medoids clustering and select the cluster medoids.
#'
#' @inheritParams step_sbf
#' @param k number of k-medoids clusterings of the variables.  The value of
#'   \code{k} is constrained to be between 1 and one less than the number of
#'   original variables.
#' @param center,scale logicals indicating whether to mean center and median
#'   absolute deviation scale the original variables prior to cluster
#'   partitioning, or functions or names of functions for the centering and
#'   scaling; not applied to selected variables.
#' @param method character string specifying one of the clustering methods
#'   provided by the \pkg{cluster} package.  The \code{clara} (clustering
#'   large applications) method is an extension of \code{pam} (partitioning
#'   around medoids) designed to handle large datasets.
#' @param metric character string specifying the distance metric for calculating
#'   dissimilarities between observations as \code{"euclidean"},
#'   \code{"manhattan"}, or \code{"jaccard"} (\code{clara} only).
#' @param optimize logical indicator or 0:5 integer level specifying
#'   optimization for the \code{\link[cluster]{pam}} clustering method.
#' @param num_samp number of sub-datasets to sample for the
#'   \code{\link[cluster]{clara}} clustering method.
#' @param samp_size number of cases to include in each sub-dataset.
#' @param x \code{step_kmedoids} object.
#'
#' @return Function \code{step_kmedoids} creates a new step whose class is of
#' the same name and inherits from \code{\link{step_sbf}}, adds it to the
#' sequence of existing steps (if any) in the recipe, and returns the updated
#' recipe.  For the \code{tidy} method, a tibble with columns \code{terms}
#' (selectors or variables selected), \code{cluster} assignments,
#' \code{selected} (logical indicator of selected cluster medoids),
#' \code{silhouette} (silhouette values), and \code{name} of the selected
#' variable names.
#'
#' @details
#' K-medoids clustering partitions variables into k groups such that the
#' dissimilarity between the variables and their assigned cluster medoids is
#' minimized.  Cluster medoids are then returned as a set of k variables.
#'
#' @references
#' Kaufman L and Rousseeuw PJ (1990). Finding Groups in Data: An Introduction to
#' Cluster Analysis. Wiley: New York.
#'
#' Reynolds A, Richards G, de la Iglesia B and Rayward-Smith V (1992).
#' Clustering rules: a comparison of partitioning and hierarchical clustering
#' algorithms. Journal of Mathematical Modelling and Algorithms 5, 475--504.
#'
#' @seealso \code{\link[cluster]{pam}}, \code{\link[cluster]{clara}},
#' \code{\link[recipes]{recipe}}, \code{\link[recipes]{prep}},
#' \code{\link[recipes]{bake}}
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(rating ~ ., data = attitude)
#' kmedoids_rec <- rec %>%
#'   step_kmedoids(all_predictors(), k = 3)
#' kmedoids_prep <- prep(kmedoids_rec, training = attitude)
#' kmedoids_data <- bake(kmedoids_prep, attitude)
#'
#' pairs(kmedoids_data, lower.panel = NULL)
#'
#' tidy(kmedoids_rec, number = 1)
#' tidy(kmedoids_prep, number = 1)
#'
step_kmedoids <- function(recipe, ..., k = 5, center = TRUE, scale = TRUE,
                          method = c("pam", "clara"), metric = "euclidean",
                          optimize = FALSE, num_samp = 50,
                          samp_size = 40 + 2 * k, replace = TRUE,
                          prefix = "KMedoids", role = "predictor",
                          skip = FALSE, id = recipes::rand_id("kmedoids")) {

  recipes::add_step(recipe, new_step_kmedoids(
    terms = recipes::ellipse_check(...),
    k = k,
    center = center,
    scale = scale,
    method = match.arg(method),
    metric = metric,
    optimize = optimize,
    num_samp = num_samp,
    samp_size = samp_size,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  ))

}


new_step_kmedoids <- function(..., k, center, scale, method, metric, optimize,
                              num_samp, samp_size) {

  require_namespaces("cluster")

  filter <- function(x, y, step) {

    require_namespaces("cluster")

    if (ncol(x) < 2) {
      stop("step_kmedoids requires 2 or more variables", call. = FALSE)
    }
    recipes::check_type(x)

    stats <- map(function(stat) {
      if (is.function(stat)) apply(x, 2, stat) else stat
    }, step[c("center", "scale")])
    x <- t(base::scale(x, center = stats$center, scale = stats$scale))
    k <- max(min(step$k, nrow(x) - 1), 1)
    switch(step$method,
      "pam" = {
        res <- cluster::pam(x, k, metric = step$metric, pamonce = step$optimize,
                            keep.diss = FALSE, keep.data = FALSE)
      },
      "clara" = {
        samp_size <- min(step$samp_size, nrow(x))
        res <- cluster::clara(x, k, metric = step$metric,
                              samples = step$num_samp, sampsize = samp_size,
                              medoids.x = FALSE, rngR = TRUE)
        names(res)[names(res) == "i.med"] <- "id.med"
      }
    )

    tibble(
      cluster = res$clustering,
      selected = seq_along(res$clustering) %in% res$id.med,
      silhoutte = if (k == 1) NA_real_ else
        res$silinfo$widths[names(res$clustering), "sil_width"]
    )

  }

  options <- list(
    k = k,
    center = if (!is.logical(center)) fget(center) else
      if (center) base::mean else FALSE,
    scale = if (!is.logical(scale)) fget(scale) else
      if (scale) stats::mad else FALSE,
    method = method
  )
  switch(options$method,
    "pam" = {
      options$metric <- match.arg(metric, c("euclidean", "manhattan"))
      options$optimize <- optimize
    },
    "clara" = {
      options$metric <- match.arg(metric,
                                  c("euclidean", "manhattan", "jaccard"))
      options$num_samp <- num_samp
      options$samp_size <- samp_size
    },
    stop("'method' should be \"pam\" or \"clara\"")
  )

  object <- new_step_sbf(..., filter = filter, multivariate = TRUE,
                         options = options)
  object$res <- tibble(
    terms = recipes::sel2char(object$terms),
    cluster = NA_integer_,
    selected = NA,
    silhouette = NA_real_,
    name = NA_character_
  )

  structure(object, class = c("step_kmedoids", class(object)))

}


#' @rdname step_kmedoids
#'
tunable.step_kmedoids <- function(x, ...) {
  tibble(
    name = "k",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1, 10))),
    source = "MachineShop",
    component = "step_kmedoids",
    component_id = x$id
  )
}
