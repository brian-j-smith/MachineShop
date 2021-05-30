case_comp_name <- function(x, ...) {
  UseMethod("case_comp_name")
}


case_comp_name.data.frame <- function(x, type, ...) {
  name <- paste0("(", type, ")")
  if (name %in% names(x)) name
}


case_comp_name.recipe <- function(x, type, ...) {
  type <- switch(type,
    "names" = "name",
    "strata" = "stratum",
    "weights" = "weight"
  )
  role <- paste0("case_", type)
  info <- summary(x)
  name <- info$variable[info$role == role]
  if (length(name) == 1) {
    name
  } else if (length(name) > 1) {
    throw(Error("multiple ", role, " variables"))
  }
}


case_strata <- function(x, ...) {
  UseMethod("case_strata")
}


case_strata.default <- function(x, ...) {
  throw(TypeError(x, c("BinomialVariate", "character", "factor", "logical",
                       "matrix", "numeric", "Surv"), "'x'"))
}


case_strata.BinomialVariate <- function(x, ...) {
  case_strata(as.numeric(x), ...)
}


case_strata.character <- function(x, ...) {
  case_strata(as.factor(x), ...)
}


case_strata.factor <- function(x, prop = 0.1, size = 20, ...) {
  x <- sample_replace(x, is.na(x))
  min_count <- max(1, prop * length(x), size)
  while (nlevels(x) > 1 && any((counts <- table(x)) < min_count)) {
    if (is.ordered(x)) {
      i <- which.min(head(counts, -1) + tail(counts, -1))
      inds <- c(i, i + 1)
      levels(x)[inds] <- paste(levels(x)[inds], collapse = "+")
    } else {
      i <- which.min(counts)
      x <- sample_replace(x, x == levels(x)[i])
      levels(x)[i] <- NA
    }
  }
  x
}


case_strata.logical <- function(x, ...) {
  case_strata(as.factor(x), ...)
}


case_strata.matrix <- function(x, ...) {
  case_strata(x[, 1], ...)
}


case_strata.ModelFrame <- function(x, ...) {
  name <- case_strata_name(x)
  if (length(name)) case_strata(x[[name]], ...)
}


case_strata.ModelRecipe <- function(x, ...) {
  name <- case_strata_name(x)
  if (length(name)) case_strata(as.data.frame(x)[[name]], ...)
}


case_strata.numeric <- function(
  x, breaks = 4, nunique = 5, prop = 0.1, size = 20, ...
) {
  x <- sample_replace(x, is.na(x))
  if (length(unique(x)) <= max(1, nunique)) {
    x <- ordered(x)
    levels(x) <- paste0("V", seq_len(nlevels(x)))
    res <- case_strata(x, prop = prop, size = size)
  } else {
    min_count <- max(1, prop * length(x), size)
    breaks <- max(1, breaks)
    while (breaks > 0) {
      quants <- quantile(x, 0:breaks / breaks)
      res <- cut(x, unique(quants), include.lowest = TRUE)
      if (all(table(res) >= min_count)) break
      breaks <- breaks - 1
    }
  }
  res
}


case_strata.Surv <- function(
  x, breaks = 4, nunique = 5, prop = 0.1, size = 20, ...
) {
  status <- x[, "status"]
  status <- sample_replace(status, is.na(status))

  time_split <- split(time(x), status)
  strata_split <- list()
  strata_levels <- character()

  for (name in names(time_split)) {
    time <- time_split[[name]]
    res <- case_strata(time, breaks = breaks, nunique = nunique,
                       prop = prop * length(status) / length(time),
                       size = size)
    levels(res) <- paste(name, levels(res), sep = ":")
    strata_levels <- c(strata_levels, levels(res))
    strata_split[[name]] <- as.character(res)
  }

  factor(unsplit(strata_split, status), levels = strata_levels)
}


case_strata_name <- function(x) {
  case_comp_name(x, "strata")
}


case_weights <- function(x, ...) {
  UseMethod("case_weights")
}


case_weights.MLModel <- function(x, ...) {
  case_weights(x@x, ...)
}


case_weights.ModelFrame <- function(x, ...) {
  name <- case_weights_name(x)
  if (length(name)) x[[name]]
}


case_weights.ModelRecipe <- function(x, ...) {
  name <- case_weights_name(x)
  if (length(name)) juice(prep(x))[[name]]
}


case_weights_name <- function(x) {
  case_comp_name(x, "weights")
}
