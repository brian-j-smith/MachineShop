case_comps <- function(object, ...) {
  UseMethod("case_comps")
}


case_comps.MLModelFit <- function(object, ...) {
  case_comps(as.MLInput(object), ...)
}


case_comps.ModelFrame <- function(
  object, newdata = NULL, types = c("names", "weights"), response = FALSE,
  original = FALSE, ...
) {
  stopifnot(!original)
  data <- if (is.null(newdata)) object else newdata
  res <- map(function(type) {
    name <- case_comp_name(object, type)
    if (length(name)) data[[name]]
  }, types)
  if (response) res$response <- response(object, newdata)
  res
}


case_comps.recipe <- function(
  object, newdata = NULL, types = c("names", "weights"), response = FALSE,
  original = FALSE, ...
) {
  stopifnot(!original)
  names <- map(function(type) case_comp_name(object, type), types)
  data <- if (any(lengths(names)) || response) {
    bake(prep(object), newdata = newdata)
  }
  res <- map(function(type) {
    name <- names[[type]]
    if (length(name)) data[[name]]
  }, types)
  if (response) res$response <- response(terms(object), data)
  res
}


case_comp_name <- function(object, ...) {
  UseMethod("case_comp_name")
}


case_comp_name.data.frame <- function(object, type, ...) {
  name <- paste0("(", type, ")")
  if (name %in% names(object)) name
}


case_comp_name.recipe <- function(object, type, ...) {
  type <- switch(type,
    "groups" = "group",
    "names" = "name",
    "strata" = "stratum",
    "weights" = "weight",
    return(NULL)
  )
  role <- paste0("case_", type)
  info <- summary(object)
  name <- info$variable[info$role == role]
  if (length(name) == 1) {
    name
  } else if (length(name) > 1) {
    throw(Error("Multiple ", role, " variables."))
  }
}


case_groups <- function(object, ...) {
  name <- case_comp_name(object, type = "groups")
  if (length(name)) as.data.frame(object)[[name]]
}


case_strata <- function(object, ...) {
  UseMethod("case_strata")
}


case_strata.default <- function(object, ...) {
  types <- c("BinomialVariate", "character", "factor", "logical", "matrix",
             "numeric", "Surv")
  throw(TypeError(object, types, "'object'"))
}


case_strata.BinomialVariate <- function(object, ...) {
  case_strata(as.numeric(object), ...)
}


case_strata.character <- function(object, ...) {
  case_strata(as.factor(object), ...)
}


case_strata.factor <- function(object, prop = 0.1, size = 20, ...) {
  object <- sample_replace(object, is.na(object))
  min_count <- max(prop * length(object), size, 1)
  while (nlevels(object) > 1 && any((counts <- table(object)) < min_count)) {
    if (is.ordered(object)) {
      i <- which.min(head(counts, -1) + tail(counts, -1))
      inds <- c(i, i + 1)
      levels(object)[inds] <- paste(levels(object)[inds], collapse = "+")
    } else {
      i <- which.min(counts)
      object <- sample_replace(object, object == levels(object)[i])
      levels(object)[i] <- NA
    }
  }
  object
}


case_strata.logical <- function(object, ...) {
  case_strata(as.factor(object), ...)
}


case_strata.matrix <- function(object, ...) {
  case_strata(object[, 1], ...)
}


case_strata.ModelFrame <- function(object, ...) {
  name <- case_comp_name(object, "strata")
  if (length(name)) case_strata(object[[name]], ...)
}


case_strata.recipe <- function(object, ...) {
  name <- case_comp_name(object, "strata")
  if (length(name)) case_strata(as.data.frame(object)[[name]], ...)
}


case_strata.numeric <- function(
  object, breaks = 4, nunique = 5, prop = 0.1, size = 20, ...
) {
  object <- sample_replace(object, is.na(object))
  if (length(unique(object)) <= max(nunique, 1)) {
    object <- ordered(object)
    levels(object) <- make_names_len(nlevels(object), "level")
    res <- case_strata(object, prop = prop, size = size)
  } else {
    min_count <- max(prop * length(object), 1, size)
    breaks <- max(breaks, 1)
    while (breaks > 0) {
      quants <- quantile(object, 0:breaks / breaks)
      res <- cut(object, unique(quants), include.lowest = TRUE)
      if (all(table(res) >= min_count)) break
      breaks <- breaks - 1
    }
  }
  res
}


case_strata.Surv <- function(
  object, breaks = 4, nunique = 5, prop = 0.1, size = 20, ...
) {
  status <- object[, "status"]
  status <- sample_replace(status, is.na(status))

  time_split <- split(time(object), status)
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


#' Extract Case Weights
#'
#' Extract the case weights from an object.
#'
#' @param object model \link{fit} result, \code{\link{ModelFrame}}, or
#'   \code{\link[recipes]{recipe}}.
#' @param newdata dataset from which to extract the weights if given; otherwise,
#'   \code{object} is used.  The dataset should be given as a \code{ModelFrame}
#'   or as a \link[=data.frame]{data frame} if \code{object} contains a
#'   \code{ModelFrame} or a \code{recipe}, respectively.
#'
#' @examples
#' ## Training and test sets
#' inds <- sample(nrow(ICHomes), nrow(ICHomes) * 2 / 3)
#' trainset <- ICHomes[inds, ]
#' testset <- ICHomes[-inds, ]
#'
#' ## ModelFrame case weights
#' trainmf <- ModelFrame(sale_amount ~ . - built, data = trainset, weights = built)
#' testmf <- ModelFrame(formula(trainmf), data = testset, weights = built)
#' mf_fit <- fit(trainmf, model = GLMModel)
#' rmse(response(mf_fit, testmf), predict(mf_fit, testmf),
#'      case_weights(mf_fit, testmf))
#'
#' ## Recipe case weights
#' library(recipes)
#' rec <- recipe(sale_amount ~ ., data = trainset) %>%
#'   role_case(weight = built, replace = TRUE)
#' rec_fit <- fit(rec, model = GLMModel)
#' rmse(response(rec_fit, testset), predict(rec_fit, testset),
#'      case_weights(rec_fit, testset))
#'
case_weights <- function(object, newdata = NULL) {
  case_comps(object, newdata, "weights")[[1]]
}
