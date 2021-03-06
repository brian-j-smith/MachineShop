ModelRecipe <- function(object, ...) {
  UseMethod("ModelRecipe")
}


ModelRecipe.ModelRecipe <- function(object, ...) {
  object
}


ModelRecipe.recipe <- function(object, ...) {
  if (any(map_logi(function(step) isTRUE(step$trained), object$steps))) {
    throw(Error("recipe must be untrained"))
  }

  cases_name <- "(names)"
  cases_fo <- ~ -`(names)`

  reserved <- intersect(c(cases_name, "(strata)"), summary(object)$variable)
  if (length(reserved)) {
    msg <- label_items("supplied recipe contains reserved variable", reserved)
    throw(Error(msg))
  }
  cases_info <- data.frame(
    variable = cases_name,
    type = "nominal",
    role = "case_name",
    source = "original"
  )
  object$var_info <- rbind(object$var_info, cases_info)
  object$term_info <- rbind(object$term_info, cases_info)
  object$template[[cases_name]] <- rownames(object$template)

  for (i in seq_along(object$steps)) {
    step_terms <- object$steps[[i]]$terms
    environment(cases_fo) <- environment(step_terms[[1]])
    new_term <- rlang::as_quosure(cases_fo)
    object$steps[[i]]$terms <- c(step_terms, new_term)
  }

  new("ModelRecipe", object)
}


bake.ModelRecipe <- function(object, new_data, ...) {
  bake(as(object, "recipe"), new_data = prep_recipe_data(new_data))
}


bake.SelectedInput <- function(object, ...) {
  throw(Error("cannot create a design matrix from a ", class(object)))
}


bake.TunedInput <- function(object, ...) {
  throw(Error("cannot create a design matrix from a ", class(object)))
}


juice <- function(x, ...) {
  UseMethod("juice")
}


juice.default <- function(x, ...) {
  recipes::juice(x, ...)
}


juice.ModelRecipe <- function(x, ...) {
  bake(x, x$template)
}


prep.ModelFrame <- function(x, ...) x


prep.ModelRecipe <- function(x, ...) {
  if (!recipes::fully_trained(x)) {
    template <- x$template
    x <- new(class(x), prep(as(x, "recipe"), retain = FALSE))
    x$template <- template
    x$orig_lvls[["(names)"]] <- list(values = NA, ordered = NA)
    x$levels[["(names)"]] <- x$orig_lvls[["(names)"]]
  }
  x
}


prep.SelectedInput <- function(x, ...) {
  throw(Error("cannot train a ", class(x)))
}


prep.TunedInput <- function(x, ...) {
  throw(Error("cannot train a ", class(x)))
}


prep_recipe_data <- function(x) {
  if (is.null(x[["(names)"]])) x[["(names)"]] <- rownames(x)
  x[["(strata)"]] <- NULL
  x
}


recipe.ModelRecipe <- function(x, data, ...) {
  stopifnot(is(data, "data.frame"))
  x$template <- as_tibble(prep_recipe_data(data))
  x
}


update.recipe <- function(object, ...) {
  args <- list(...)
  for (i in seq_along(object$steps)) {
    step <- object$steps[[i]]
    params <- args[[step$id]]
    if (!is.null(params)) {
      object$steps[[i]] <- do.call(update, c(list(step), params))
    }
  }
  object
}
