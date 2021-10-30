#' Variable Selection by Filtering
#'
#' Creates a \emph{specification} of a recipe step that will select variables
#' from a candidate set according to a user-specified filtering function.
#'
#' @inheritParams step_lincomp
#' @param filter function whose first argument \code{x} is a univariate vector
#'   or a \code{multivariate} data frame of candidate variables from which to
#'   select, second argument \code{y} is the response variable as
#'   defined in preceding recipe steps, and third argument \code{step} is the
#'   current step.  The function should return a logical value or vector of
#'   length equal the number of variables in \code{x} indicating whether to
#'   select the corresponding variable, or return a list or data frame with
#'   element \code{`selected`} containing the logical(s) and possibly with other
#'   elements of the same length to be included in output from the \code{tidy}
#'   method.
#' @param multivariate logical indicating that candidate variables be passed to
#'   the \code{x} argument of the \code{filter} function separately as
#'   univariate vectors if \code{FALSE}, or altogether in one multivariate data
#'   frame if \code{TRUE}.
#' @param options list of elements to be added to the step object for use in the
#'   \code{filter} function.
#' @param prefix if the original variables are not replaced, the selected
#'   variables are added to the dataset with the character string prefix added
#'   to their names; otherwise, the original variable names are retained.
#' @param x \code{step_sbf} object.
#'
#' @return An updated version of \code{recipe} with the new step added to the
#' sequence of existing steps (if any).  For the \code{tidy} method, a tibble
#' with columns \code{terms} (selectors or variables selected), \code{selected}
#' (logical indicator of selected variables), and \code{name} of the selected
#' variable names.
#'
#' @seealso \code{\link[recipes]{recipe}}, \code{\link[recipes]{prep}},
#' \code{\link[recipes]{bake}}
#'
#' @examples
#' library(recipes)
#'
#' glm_filter <- function(x, y, step) {
#'   model_fit <- glm(y ~ ., data = data.frame(y, x))
#'   p_value <- drop1(model_fit, test = "F")[-1, "Pr(>F)"]
#'   p_value < step$threshold
#' }
#'
#' rec <- recipe(rating ~ ., data = attitude)
#' sbf_rec <- rec %>%
#'   step_sbf(all_numeric(), -all_outcomes(),
#'            filter = glm_filter, options = list(threshold = 0.05))
#'
#' sbf_prep <- prep(sbf_rec, training = attitude)
#' sbf_data <- bake(sbf_prep, attitude)
#'
#' pairs(sbf_data, lower.panel = NULL)
#'
#' tidy(sbf_rec, number = 1)
#' tidy(sbf_prep, number = 1)
#'
step_sbf <- function(
  recipe, ..., filter, multivariate = FALSE, options = list(), replace = TRUE,
  prefix = "SBF", role = "predictor", skip = FALSE, id = recipes::rand_id("sbf")
) {

  recipes::add_step(recipe, new_step_sbf(
    terms = recipes::ellipse_check(...),
    filter = filter,
    multivariate = multivariate,
    options = options,
    replace = replace,
    prefix = prefix,
    role = role,
    skip = skip,
    id = id
  ))

}


new_step_sbf <- function(
  terms, filter, multivariate, options, replace, prefix, role, skip, id
) {
  stopifnot(is.function(filter))
  stopifnot(is.list(options))
  step_args <- list(
    subclass = "sbf",
    terms = terms,
    filter = filter,
    multivariate = multivariate,
    res = tibble(
      terms = recipes::sel2char(terms),
      selected = NA,
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
    msg <- "options list contains reserved step name"
    throw(Error(label_items(msg, invalid_names)))
  }
  do.call(recipes::step, c(step_args, options))
}


prep.step_sbf <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  x_vars <- training[col_names]
  y_var <- response(terms.recipe_info(info), training)
  res <- if (x$multivariate) {
    x$filter(x = x_vars, y = y_var, step = x)
  } else {
    res_list <- map(x$filter, x = x_vars, y = list(y_var), step = list(x))
    if (all(map_logi(is.list, res_list))) {
      do.call(rbind, map(as_tibble, res_list))
    } else unlist(res_list)
  }
  if (!is.list(res)) res <- list(selected = res)
  if (!is.logical(res$selected)) {
    throw(Error("filter function should return logical values"))
  }
  if (length(res$selected) != length(x_vars)) {
    throw(Error("filter function should return one logical value per variable"))
  }
  x$res <- tibble(
    terms = col_names,
    as_tibble(res),
    name = ifelse(res$selected,
                  paste0(if (!x$replace) paste0(x$prefix, "."), col_names),
                  NA_character_)
  )
  x$trained <- TRUE
  x
}


bake.step_sbf <- function(object, new_data, ...) {
  res <- object$res
  if (object$replace) {
    names_drop <- res$terms[!res$selected]
    new_data[!(names(new_data) %in% names_drop)]
  } else {
    res <- res[res$selected, c("terms", "name")]
    selected_vars <- recipes::check_name(new_data[res$terms], new_data, object,
                                         newname = res$name)
    as_tibble(c(new_data, selected_vars))
  }
}


print.step_sbf <- function(x, width = getOption("width"), ...) {
  msg <- paste(x$prefix, "selection by filtering for ")
  width <- max(width - nchar(msg), 20)
  cat(msg)
  recipes::printer(x$res$terms, x$terms, x$trained, width = width)
  invisible(x)
}


#' @rdname step_sbf
#'
tidy.step_sbf <- function(x, ...) {
  res <- x$res
  res$id <- x$id
  res
}
