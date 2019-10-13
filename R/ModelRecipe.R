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
  
  structure(object, class = c("ModelRecipe", "recipe"))
}


as.data.frame.ModelRecipe <- function(x, original = TRUE, ...) {
  as.data.frame(if (original) x$template else juice(prep(x)))
}


bake.ModelRecipe <- function(object, new_data, ...) {
  bake(structure(object, class = "recipe"), prep_recipe_data(new_data))
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
    x_class <- class(x)
    class(x) <- "recipe"
    structure(prep(x, retain = FALSE), class = x_class)
  } else x
}


recipe.ModelRecipe <- function(x, data, ...) {
  if (fully_trained(x)) {
    x <- prep(x, training = data, fresh = TRUE, retain = FALSE)
  }
  x$template <- as_tibble(prep_recipe_data(data))
  x
}


prep_recipe_data <- function(x) {
  case_name_var <- "(casenames)"
  if (is.null(x[[case_name_var]])) x[[case_name_var]] <- rownames(x)
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
