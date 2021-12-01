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
  x <- as.character(substitute(x))
  size <- as.character(substitute(size))
  if (nzchar(x) && nzchar(size)) {
    recipes::add_role(recipe, x, new_role = "binom_x") %>%
      recipes::add_role(size, new_role = "binom_size")
  } else {
    throw(Error("Binomial 'x' and 'size' variables must be specified."))
  }
}


#' @rdname recipe_roles
#'
role_case <- function(recipe, group, stratum, weight, replace = FALSE) {
  comp_names <- map(
    as.character,
    eval(substitute(alist(
      group = group,
      stratum = stratum,
      weight = weight
    )))
  )
  f <- if (replace) recipes::update_role else recipes::add_role
  for (type in names(comp_names)) {
    name <- comp_names[[type]]
    if (nzchar(name)) {
      recipe <- f(recipe, name, new_role = paste0("case_", type))
    }
  }
  recipe
}


#' @rdname recipe_roles
#'
role_pred <- function(recipe, offset, replace = FALSE) {
  offset <- as.character(substitute(offset))
  f <- if (replace) recipes::update_role else recipes::add_role
  if (nzchar(offset)) f(recipe, offset, new_role = "pred_offset") else recipe
}


#' @rdname recipe_roles
#'
role_surv <- function(recipe, time, event) {
  time <- as.character(substitute(time))
  if (nzchar(time)) {
    recipe <- recipes::add_role(recipe, time, new_role = "surv_time")
    event <- as.character(substitute(event))
    if (nzchar(event)) {
      recipe <- recipes::add_role(recipe, event, new_role = "surv_event")
    }
    recipe
  } else {
    throw(Error("A survival 'time' variable must be specified."))
  }
}
