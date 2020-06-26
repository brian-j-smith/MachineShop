#' K-Medoids Clustering Variable Selection
#'
#' Creates a \emph{specification} of a recipe step that will partition numeric
#' variables according to k-medoids clustering and select the cluster medoids.
#'
#' @inheritParams step_kmeans
#' @param k number of k-medoids clusterings of the variables.  The value of
#'   \code{k} is constrained to be between 1 and one less than the number of
#'   original variables.
#' @param center,scale logicals indicating whether to mean center and median
#'   absolute deviation scale the original variables prior to cluster
#'   partitioning; not applied to selected variables.
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
#' @param prefix if the original variables are not replaced, a character string
#'   prefix added to a sequence of zero-padded integers to generate names for
#'   the resulting new variables; otherwise, the original variable names are
#'   retained.
#' @param x \code{step_kmedoids} object.
#'
#' @return An updated version of \code{recipe} with the new step added to the
#' sequence of existing steps (if any).  For the \code{tidy} method, a tibble
#' with columns \code{terms} (selectors or variables selected), \code{cluster}
#' assignments, \code{medoid} (logical indicator of cluster medoids),
#' \code{silhouette} (silhouette values), and \code{names} for the new
#' variables.
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
                          method = c("pam", "clara"),
                          metric = "euclidean", optimize = FALSE,
                          num_samp = 50, samp_size = 40 + 2 * k,
                          replace = TRUE, prefix = "KMedoids",
                          role = "predictor", skip = FALSE,
                          id = recipes::rand_id("kmedoids")) {

  requireModelNamespaces("cluster")

  step <- recipes::step(
    subclass = "kmedoids",
    terms = recipes::ellipse_check(...),
    k = k,
    center = if (isTRUE(center)) base::mean else FALSE,
    scale = if (isTRUE(scale)) stats::mad else FALSE,
    method = match.arg(method),
    res = list(),
    trained = FALSE,
    replace = replace,
    prefix = if (!replace) prefix,
    role = role,
    skip = skip,
    id = id
  )
  switch(step$method,
         "pam" = {
           step$metric <- match.arg(metric, c("euclidean", "manhattan"))
           step$optimize <- optimize
         },
         "clara" = {
           step$metric <- match.arg(metric,
                                    c("euclidean", "manhattan", "jaccard"))
           step$num_samp <- num_samp
           step$samp_size <- samp_size
         })
  recipes::add_step(recipe, step)

}


prep.step_kmedoids <- function(x, training, info = NULL, ...) {

  requireModelNamespaces("cluster")

  col_names <- recipes::terms_select(terms = x$terms, info = info)
  if (length(col_names) < 2) {
    stop("step_kmedoids requires 2 or more variables", call. = FALSE)
  }

  training <- training[col_names]
  recipes::check_type(training)

  x$k <- max(min(x$k, length(col_names) - 1), 1)

  if (is.function(x$center)) x$center <- apply(training, 2, x$center)
  if (is.function(x$scale)) x$scale <- apply(training, 2, x$scale)
  training <- t(scale(training, center = x$center, scale = x$scale))

  res <- switch(x$method,
                "pam" = {
                  keep <- c("id.med", "clustering", "sil_width")
                  cluster::pam(training, k = x$k, metric = x$metric,
                               pamonce = x$optimize, keep.diss = FALSE,
                               keep.data = FALSE)[keep]
                },
                "clara" = {
                  x$samp_size <- min(x$samp_size, nrow(training))
                  keep <- c("i.med", "clustering", "sil_width")
                  cluster::clara(training, k = x$k, metric = x$metric,
                                 samples = x$num_samp, sampsize = x$samp_size,
                                 medoids.x = FALSE, rngR = TRUE)[keep]
                })
  names(res) <- c("id.med", "clustering", "sil_width")

  names(res$id.med) <- if (x$replace) {
    names(res$clustering)[res$id.med]
  } else {
    recipes::names0(length(res$id.med), x$prefix)
  }

  x$res <- res[c("id.med", "clustering")]
  x$silhoutte <- res$silinfo$widths[, "sil_width"]
  x$trained <- TRUE
  x

}


bake.step_kmedoids <- function(object, new_data, ...) {
  res <- object$res
  var_names <- names(res$clustering)
  if (object$replace) {
    names_drop <- var_names[-res$id.med]
    new_data[!(names(new_data) %in% names_drop)]
  } else {
    cluster_medoids <- new_data[var_names[res$id.med]]
    names(cluster_medoids) <- names(res$id.med)
    cluster_medoids <- recipes::check_name(cluster_medoids, new_data, object)
    as_tibble(c(new_data, cluster_medoids))
  }
}


print.step_kmedoids <- function(x, width = max(20, options()$width - 33), ...) {
  cat("K-medoids cluster extraction for ")
  recipes::printer(names(x$res$clustering), x$terms, x$trained, width = width)
  invisible(x)
}


#' @rdname step_kmedoids
#'
tidy.step_kmedoids <- function(x, ...) {
  cluster <- x$res$clustering
  medoid_ind <- x$res$id.med
  if (is.trained(x)) {
    res <- tibble(
      terms = names(cluster),
      cluster = cluster,
      medoid = FALSE
    )
    res$medoid[medoid_ind] <- TRUE
    res$silhouette <- x$res$silhouette[res$terms]
    res$names <- names(medoid_ind)[cluster]
  } else {
    res <- tibble(
      terms = recipes::sel2char(x$terms),
      cluster = NA_integer_,
      medoid = NA,
      silhouette = NA_real_,
      names = NA_character_
    )
  }
  res$id <- x$id
  res
}


#' @rdname step_kmedoids
#'
tunable.step_kmedoids <- function(x, ...) {
  tibble(
    name = "k",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(2, 10))),
    source = "MachineShop",
    component = "step_kmedoids",
    component_id = x$id
  )
}
