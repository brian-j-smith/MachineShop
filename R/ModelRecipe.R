ModelRecipe <- function(object, ...) {
  UseMethod("ModelRecipe")
}


ModelRecipe.ModelRecipe <- function(object, ...) {
  object
}


ModelRecipe.recipe <- function(object, ...) {
  if (any(map_logi(function(step) isTRUE(step$trained), object$steps))) {
    stop("recipe must be untrained")
  }

  case_name_var <- "(names)"
  case_name_fo <- ~ -`(names)`

  if (case_name_var %in% summary(object)$variable) {
    stop("conflict with existing recipe variable: ", case_name_var)
  }
  case_name_info <- data.frame(
    variable = case_name_var,
    type = "nominal",
    role = "case_name",
    source = "original"
  )
  object$var_info <- rbind(object$var_info, case_name_info)
  object$term_info <- rbind(object$term_info, case_name_info)
  object$template[[case_name_var]] <- rownames(object$template)

  for (i in seq(object$steps)) {
    step_terms <- object$steps[[i]]$terms
    environment(case_name_fo) <- environment(step_terms[[1]])
    new_term <- rlang::as_quosure(case_name_fo)
    object$steps[[i]]$terms <- c(step_terms, new_term)
  }

  new("ModelRecipe", object)
}


bake.ModelRecipe <- function(object, new_data, ...) {
  bake(as(object, "recipe"), new_data = prep_recipe_data(new_data))
}


bake.SelectedInput <- function(object, ...) {
  stop("cannot create a design matrix from a ", class(object))
}


bake.TunedInput <- function(object, ...) {
  stop("cannot create a design matrix from a ", class(object))
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
    x <- new(class(x), prep(as(x, "recipe"), retain = FALSE))
    case_name_var <- "(names)"
    x$template[[case_name_var]] <- as.character(x$template[[case_name_var]])
    x$orig_lvls[[case_name_var]] <- list(values = NA, ordered = NA)
    x$levels[[case_name_var]] <- x$orig_lvls[[case_name_var]]
  }
  x
}


prep.SelectedInput <- function(x, ...) {
  stop("cannot train a ", class(x))
}


prep.TunedInput <- function(x, ...) {
  stop("cannot train a ", class(x))
}


prep_recipe_data <- function(x) {
  case_name_var <- "(names)"
  if (is.null(x[[case_name_var]])) x[[case_name_var]] <- rownames(x)
  x
}


recipe.ModelRecipe <- function(x, data, ...) {
  stopifnot(is(data, "data.frame"))
  x$template <- as_tibble(prep_recipe_data(data))
  x
}


update.recipe <- function(object, ...) {
  args <- list(...)
  for (i in seq(object$steps)) {
    step <- object$steps[[i]]
    params <- args[[step$id]]
    if (!is.null(params)) {
      object$steps[[i]] <- do.call(update, c(list(step), params))
    }
  }
  object
}
