ModelRecipe <- function(object, ...) {
  UseMethod("ModelRecipe")
}


ModelRecipe.ModelRecipe <- function(object, ...) {
  object
}


ModelRecipe.recipe <- function(object, ...) {
  if (any(map("logi", function(step) isTRUE(step$trained), object$steps))) {
    throw(Error("Recipe must be untrained."))
  }

  cases_name <- "(names)"
  cases_fo <- ~ -`(names)`

  reserved <- intersect(
    c("(groups)", "(names)", "(strata)"), summary(object)$variable
  )
  if (length(reserved)) {
    throw(Error(note_items(
      "Supplied recipe contains reserved variable{?s}: ", reserved, "."
    )))
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


bake.ModelRecipe <- function(object, new_data, newdata = NULL, ...) {
  if (missing(new_data)) {
    new_data <- if (is.null(newdata)) {
      as.data.frame(object)
    } else if (is(newdata, "ModelRecipe")) {
      as.data.frame(newdata)
    } else {
      prep_recipe_data(newdata)
    }
  }
  bake(as(object, "recipe"), new_data)
}


bake.SelectedInput <- function(object, ...) {
  throw(Error("Cannot create a design matrix from a ", class(object), "."))
}


bake.TunedInput <- function(object, ...) {
  throw(Error("Cannot create a design matrix from a ", class(object), "."))
}


prep.ModelFrame <- function(x, ...) x


prep.ModelRecipe <- function(x, retain = TRUE, ...) {
  if (!is_trained(x)) {
    template <- x$template
    x <- new(class(x), prep(as(x, "recipe"), retain = retain))
    x$orig_template <- template
    x$orig_lvls[["(names)"]] <- list(values = NA, ordered = NA)
    x$levels[["(names)"]] <- x$orig_lvls[["(names)"]]
  } else if (!retain) {
    x$template <- x$template[NULL, ]
    x$retained <- FALSE
  } else if (!x$retained) {
    throw(Error(
      "ModelRecipe prep cannot be updated from 'retain = FALSE' to 'TRUE'."
    ))
  }
  x
}


prep.SelectedInput <- function(x, ...) {
  throw(Error("Cannot train a ", class(x), "."))
}


prep.TunedInput <- function(x, ...) {
  throw(Error("Cannot train a ", class(x), "."))
}


prep_recipe_data <- function(x) {
  if (is.null(x[["(names)"]])) x[["(names)"]] <- rownames(x)
  x[c("(groups)", "(strata)")] <- NULL
  as_tibble(x)
}


update.ModelRecipe <- function(
  object, params = NULL, data = NULL, new_id = FALSE, ...
) {
  stopifnot(!is_trained(object))
  if (is.list(params)) {
    for (i in seq_along(object$steps)) {
      step <- object$steps[[i]]
      step_params <- params[[step$id]]
      if (length(step_params)) {
        object$steps[[i]] <- do.call(update, c(list(step), step_params))
      }
    }
  }
  if (is.data.frame(data)) object$template <- prep_recipe_data(data)
  if (is.character(new_id)) {
    object@id <- new_id
  } else if (isTRUE(new_id)) {
    object@id <- make_id()
  }
  object
}
