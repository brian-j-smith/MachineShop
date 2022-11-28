#' Set Recipe Roles
#'
#' Add to or replace the roles of variables in a preprocessing recipe.
#'
#' @name recipe_roles
#' @rdname recipe_roles
#'
#' @param recipe existing \link[recipes]{recipe} object.
#' @param x,size number of counts and trials for the specification of a
#'   \code{\link{BinomialVariate}} outcome.
#' @param group variable defining groupings of cases to keep together when
#'   folds are constructed for \link[=controls]{cross-validation}
#'   [default: none].
#' @param stratum variable to use in conducting stratified \link{resample}
#'   estimation of model performance.
#' @param weight numeric variable of case weights for model
#'   \link[=fit]{fitting}.
#' @param replace logical indicating whether to replace existing roles.
#' @param offset numeric variable to be added to a linear predictor, such as in
#'   a generalized linear model, with known coefficient 1 rather than an
#'   estimated coefficient.
#' @param time,event numeric follow up time and 0-1 numeric or logical event
#'   indicator for specification of a \code{\link[survival]{Surv}} outcome.  If
#'   the event indicator is omitted, all cases are assumed to have events.
#'
#' @return An updated recipe object.
#'
#' @seealso \code{\link[recipes]{recipe}}
#'
#' @examples
#' library(survival)
#' library(recipes)
#'
#' df <- within(veteran, {
#'   y <- Surv(time, status)
#'   remove(time, status)
#' })
#' rec <- recipe(y ~ ., data = df) %>%
#'   role_case(stratum = y)
#'
#' (res <- resample(rec, model = CoxModel))
#' summary(res)
#'
NULL


#' @rdname recipe_roles
#'
role_binom <- function(recipe, x, size) {
  if (!(missing(x) || missing(size))) {
    recipe <- do.call(
      recipes::add_role, list(recipe, substitute(x), new_role = "binom_x")
    )
    recipe <- do.call(
      recipes::add_role, list(recipe, substitute(size), new_role = "binom_size")
    )
  } else {
    throw(Error("Binomial `x` and `size` variables must be specified."))
  }
}


#' @rdname recipe_roles
#'
role_case <- function(recipe, group, stratum, weight, replace = FALSE) {
  f <- if (replace) recipes::update_role else recipes::add_role
  comp_names <- eval(substitute(alist(
    group = group,
    stratum = stratum,
    weight = weight
  )))
  for (type in names(comp_names)[nzchar(comp_names)]) {
    recipe <- do.call(
      f, list(recipe, comp_names[[type]], new_role = paste0("case_", type))
    )
  }
  recipe
}


#' @rdname recipe_roles
#'
role_pred <- function(recipe, offset, replace = FALSE) {
  if (!missing(offset)) {
    recipe <- do.call(
      if (replace) recipes::update_role else recipes::add_role,
      list(recipe, substitute(offset), new_role = "pred_offset")
    )
  }
  recipe
}


#' @rdname recipe_roles
#'
role_surv <- function(recipe, time, event) {
  if (!missing(time)) {
    recipe <- do.call(
      recipes::add_role, list(recipe, substitute(time), new_role = "surv_time")
    )
    if (!missing(event)) {
      recipe <- do.call(
        recipes::add_role,
        list(recipe, substitute(event), new_role = "surv_event")
      )
    }
    recipe
  } else {
    throw(Error("A survival `time` variable must be specified."))
  }
}
