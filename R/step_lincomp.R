#' Linear Components Variable Reduction
#'
#' Creates a \emph{specification} of a recipe step that will compute one or more
#' linear combinations of a set of numeric variables according to a
#' user-specified transformation matrix.
#'
#' @param recipe \link[recipes]{recipe} object to which the step will be added.
#' @param ... one or more selector functions to choose which variables will be
#'   used to compute the components.  See \code{\link[recipes]{selections}} for
#'   more details.  These are not currently used by the \code{tidy} method.
#' @param transform function whose first argument \code{x} is a matrix of
#'   variables with which to compute linear combinations and second argument
#'   \code{step} is the current step.  The function should return a
#'   transformation \code{\link{matrix}} or \code{\link[Matrix]{Matrix}} of
#'   variable weights in its columns, or return a list with element
#'   \code{`weights`} containing the transformation matrix and possibly with
#'   other elements to be included as attributes in output from the \code{tidy}
#'   method.
#' @param num_comp number of components to derive.  The value of \code{num_comp}
#'   will be constrained to a minimum of 1 and maximum of the number of original
#'   variables when \code{\link[recipes]{prep}} is run.
#' @param options list of elements to be added to the step object for use in the
#'   \code{transform} function.
#' @param center,scale logicals indicating whether to mean center and standard
#'   deviation scale the original variables prior to deriving components, or
#'   functions or names of functions for the centering and scaling.
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
#' @param x \code{step_lincomp} object.
#'
#' @return An updated version of \code{recipe} with the new step added to the
#' sequence of existing steps (if any).  For the \code{tidy} method, a tibble
#' with columns \code{terms} (selectors or variables selected), \code{weight}
#' of each variable in the linear transformations, and \code{name} of the new
#' variable names.
#'
#' @seealso \code{\link[recipes]{recipe}}, \code{\link[recipes]{prep}},
#' \code{\link[recipes]{bake}}
#'
#' @examples
#' library(recipes)
#'
#' pca_mat <- function(x, step) {
#'   prcomp(x)$rotation[, 1:step$num_comp, drop = FALSE]
#' }
#'
#' rec <- recipe(rating ~ ., data = attitude)
#' lincomp_rec <- rec %>%
#'   step_lincomp(all_numeric(), -all_outcomes(),
#'                transform = pca_mat, num_comp = 3, prefix = "PCA")
#'
#' lincomp_prep <- prep(lincomp_rec, training = attitude)
#' lincomp_data <- bake(lincomp_prep, attitude)
#'
#' pairs(lincomp_data, lower.panel = NULL)
#'
#' tidy(lincomp_rec, number = 1)
#' tidy(lincomp_prep, number = 1)
#'
step_lincomp <- function(
  recipe, ..., transform, num_comp = 5, options = list(), center = TRUE,
  scale = TRUE, replace = TRUE, prefix = "LinComp", role = "predictor",
  skip = FALSE, id = recipes::rand_id("lincomp")
) {

  recipes::add_step(recipe, new_step_lincomp(
    terms = recipes::ellipse_check(...),
    transform = transform,
    num_comp = num_comp,
    options = options,
    center = center,
    scale = scale,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  ))

}


new_step_lincomp <- function(
  terms, transform, num_comp, options, center, scale, replace, prefix, role,
  skip, id
) {
  stopifnot(is.function(transform))
  stopifnot(is.list(options))
  step_args <- list(
    subclass = "lincomp",
    terms = terms,
    transform = transform,
    num_comp = num_comp,
    center = if (!is.logical(center)) fget(center) else
      if (center) base::mean else FALSE,
    scale = if (!is.logical(scale)) fget(scale) else
      if (scale) stats::sd else FALSE,
    res = tibble(
      terms = recipes::sel2char(terms),
      weight = NA_real_,
      name = NA_character_
    ),
    trained = FALSE,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  )
  invalid_names <- intersect(names(options), names(step_args))
  if (length(invalid_names)) {
    stop(label_items("options list contains reserved step name", invalid_names))
  }
  do.call(recipes::step, c(step_args, options))
}


prep.step_lincomp <- function(x, training, info = NULL, ...) {

  col_names <- recipes::terms_select(terms = x$terms, info = info)
  training <- training[col_names]
  recipes::check_type(training)

  if (is.function(x$center)) x$center <- apply(training, 2, x$center)
  if (is.function(x$scale)) x$scale <- apply(training, 2, x$scale)
  training <- scale(training, center = x$center, scale = x$scale)

  x$num_comp <- max(min(x$num_comp, ncol(training)), 1)

  res <- x$transform(x = training, step = x)
  if (!is.list(res)) res <- list(weights = res)
  if (!(is(res$weights, "matrix") || is(res$weights, "Matrix"))) {
    stop("transform matrix should return a matrix or Matrix object")
  }
  if (nrow(res$weights) != ncol(training)) {
    stop("transform matrix row length should equal the number of variables")
  }
  dimnames(res$weights) <- list(
    terms = colnames(training),
    names = recipes::names0(ncol(res$weights), x$prefix)
  )
  x$res <- res
  x$trained <- TRUE
  x

}


bake.step_lincomp <- function(object, new_data, ...) {

  weights <- object$res$weights
  is_lincomp_var <- names(new_data) %in% rownames(weights)

  lincomp_data <- scale(new_data[is_lincomp_var],
                        center = object$center, scale = object$scale)
  comps <- as_tibble(as.matrix(lincomp_data %*% weights))

  if (object$replace) new_data <- new_data[!is_lincomp_var]
  comps <- recipes::check_name(comps, new_data, object,
                               newname = colnames(weights))

  as_tibble(c(new_data, comps))

}


print.step_lincomp <- function(x, width = getOption("width"), ...) {
  msg <- paste(x$prefix, "variable reduction for ")
  width <- max(20, width - nchar(msg))
  cat(msg)
  recipes::printer(rownames(x$res$weights), x$terms, x$trained, width = width)
  invisible(x)
}


#' @rdname step_lincomp
#'
tidy.step_lincomp <- function(x, ...) {
  res <- x$res
  if (is_trained(x)) {
    res_attrs <- res[names(res) != "weights"]
    res <- res$weights %>% as.matrix %>% as.table %>%
      as.data.frame(responseName = "weight", stringsAsFactors = FALSE)
    res <- tibble(res[c("terms", "weight")], name = res$names)
    attributes(res) <- c(attributes(res), res_attrs)
  }
  res$id <- x$id
  res
}


#' @rdname step_lincomp
#'
tunable.step_lincomp <- function(x, ...) {
  tibble(
    name = c("num_comp"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 4))
    ),
    source = "MachineShop",
    component = "step_lincomp",
    component_id = x$id
  )
}
