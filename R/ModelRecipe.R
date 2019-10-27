ModelRecipe <- function(object, ...) {
  UseMethod("ModelRecipe")
}


ModelRecipe.ModelRecipe <- function(object, ...) {
  object
}


ModelRecipe.recipe <- function(object, ...) {
  if (any(sapply(object$steps, function(step) isTRUE(step$trained)))) {
    stop("recipe must be untrained")
  }
  
  case_name_var <- "(casenames)"
  case_name_fo <- ~ -`(casenames)`
  
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


setAs("ModelRecipe", "recipe",
  function(from) asS3(from)
)


as.data.frame.ModelRecipe <- function(x, original = TRUE, ...) {
  as.data.frame(if (original) x$template else juice(prep(x)))
}


bake.ModelRecipe <- function(object, new_data, ...) {
  bake(as(object, "recipe"), new_data = prep_recipe_data(new_data))
}


bake.SelectedRecipe <- function(object, ...) {
  stop("cannot create a design matrix from a ", class(object))
}


bake.TunedRecipe <- function(object, ...) {
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


prep.ModelRecipe <- function(x, ...) {
  if (!fully_trained(x)) {
    new(class(x), prep(as(x, "recipe"), retain = FALSE))
  } else x
}


prep.SelectedRecipe <- function(x, ...) {
  stop("cannot train a ", class(x))
}


prep.TunedRecipe <- function(x, ...) {
  stop("cannot train a ", class(x))
}


prep_recipe_data <- function(x) {
  case_name_var <- "(casenames)"
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
