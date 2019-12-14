#' Model Training
#'
#' Predictive performance-based selection from candidate sets or tuning over
#' grids of parameters values.
#'
#' @rdname train
#'
#' @param x object to train.
#' @param model \link[=models]{model} call.
#' @param ... arguments passed to \code{\link{resample}} in the \code{MLModel}
#' method or to other methods otherwise.
#'
#' @return All methods return a list with a trained \code{MLModel} in component
#' \code{model}.  The \code{MLModel} method additionally returns ellipsis
#' argument(s) supplied to it as one of the following components:
#' \describe{
#'   \item{x}{\code{ModelFrame} or \code{ModelRecipe}.}
#'   \item{x,data}{formula and data frame pair.}
#'   \item{x,y}{predictor matrix and response vector pair.}
#' }
#' The other methods return list component \code{x} as a trained
#' \code{ModelFrame} or \code{ModelRecipe}.
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
#' @noRd
#'
train <- function(x, ...) {
  UseMethod("train")
}


#' @rdname train
#'
train.MLModel <- function(x, ...) {

  if (is(x, "SelectedModel")) {
    params <- x@params
    models <- params$models
    grid <- tibble(Index = seq(models))
  } else if (is(x, "TunedModel")) {
    params <- x@params
    grid <- as.grid(params$grid, fixed = params$fixed,
                    ..., model = getMLObject(params$model, "MLModel"))
    models <- expand_model(list(params$model, grid))
  } else {
    return(train(SelectedModel(x), ...))
  }

  metrics <- params$metrics
  metric <- if (!is.null(metrics)) getMLObject(c(metrics)[[1]], "MLMetric")
  stat <- fget(params$stat)

  perf_list <- list()
  perf_stats <- numeric()
  for (name in names(models)) {
    res <- try(
      resample(models[[name]], ..., control = params$control),
      silent = TRUE
    )

    if (is(res, "try-error")) {
      warn("train resampling failed for ", name, " with error:\n",
           attr(res, "condition")$message)
      perf_list[[name]] <- NA
      perf_stats[name] <- NA
      next
    }

    if (is.null(metrics)) {
      method <- fget(findS3Method(performance, res$Observed))
      metrics <- c(eval(formals(method)$metrics))
      is_defined <- sapply(metrics, function(metric) {
        info <- metricinfo(metric)[[1]]
        any(mapply(is, list(res$Observed), info$response_types$observed) &
              mapply(is, list(res$Predicted), info$response_types$predicted))
      })
      metrics <- metrics[is_defined]
      metric <- getMLObject(metrics[[1]], "MLMetric")
    }

    perf <- performance(res, metrics = metrics, cutoff = params$cutoff)
    perf_list[[name]] <- perf
    perf_stats[name] <- stat(na.omit(perf[, 1]))
  }

  failed <- is.na(perf_list)
  if (all(failed)) {
    stop("train resampling failed for all models", call. = FALSE)
  } else if (any(failed)) {
    perf[] <- NA
    perf_list[failed] <- list(perf)
  }

  perf <- do.call(c, perf_list)
  selected <- ifelse(metric@maximize, which.max, which.min)(perf_stats)

  model <- models[[selected]]
  model@trainbits <- TrainBits(
    grid = grid,
    performance = perf,
    selected = structure(selected, names = colnames(perf)[1]),
    values = perf_stats,
    metric = metric
  )

  inputs <- function(x, ...) UseMethod("inputs")
  inputs.default <- function(x, ...) list(x = x)
  inputs.formula <- function(x, data, ...) list(x = x, data = data)
  inputs.matrix <- function(x, y, ...) list(x = x, y = y)

  c(inputs(...), model = model)

}


train.ModelFrame <- function(x, model, ...) {
  list(x = x, model = model)
}


train.recipe <- function(x, model, ...) {
  list(x = prep(ModelRecipe(x)), model = model)
}


#' @rdname train
#'
train.SelectedModelFrame <- function(x, model, ...) {

  last_trainbits <- model@trainbits
  if (is(model, "SelectedModel") || is(model, "TunedModel")) {
    model@params[names(x@params)] <- x@params
  } else {
    model <- do.call(SelectedModel, c(model, x@params))
  }
  terms <- x@terms

  mf <- as(x, "ModelFrame")

  trained_models <- list()
  num_models <- integer()
  for (i in seq(terms)) {
    attr(mf, "terms") <- terms[[i]]
    trained_model <- as.MLModel(fit(model, mf))
    num_models[i] <- nrow(trained_model@trainbits@grid)
    trained_model@trainbits@grid <- tibble(
      Model = tibble(Index = seq_len(num_models[i])),
      ModelFrame = tibble(Index = rep(i, num_models[i]))
    )
    trained_models[[i]] <- trained_model
  }

  trainbits <- do.call(c, lapply(trained_models, slot, name = "trainbits"))
  selected <- max(which(trainbits@selected > c(0, cumsum(num_models))))
  attr(mf, "terms") <- terms[[selected]]
  model <- trained_models[[selected]]
  model@trainbits <- if (is.null(last_trainbits)) trainbits else last_trainbits

  list(x = mf, model = model)

}


#' @rdname train
#'
train.SelectedRecipe <- function(x, model, ...) {

  last_trainbits <- model@trainbits
  if (is(model, "SelectedModel") || is(model, "TunedModel")) {
    model@params[names(x@params)] <- x@params
  } else {
    model <- do.call(SelectedModel, c(model, x@params))
  }
  recipes <- x@recipes

  data <- as.data.frame(x)
  setdata <- function(x) recipe(x, data[unique(summary(x)$variable)])

  trained_models <- list()
  num_models <- integer()
  for (i in seq(recipes)) {
    rec <- setdata(recipes[[i]])
    trained_model <- as.MLModel(fit(model, rec))
    num_models[i] <- nrow(trained_model@trainbits@grid)
    trained_model@trainbits@grid <- tibble(
      Model = tibble(Index = seq_len(num_models[i])),
      Recipe = tibble(Index = rep(i, num_models[i]))
    )
    trained_models[[i]] <- trained_model
  }

  trainbits <- do.call(c, lapply(trained_models, slot, name = "trainbits"))
  selected <- max(which(trainbits@selected > c(0, cumsum(num_models))))
  recipe <- setdata(recipes[[selected]])
  model <- trained_models[[selected]]
  model@trainbits <- if (is.null(last_trainbits)) trainbits else last_trainbits

  list(x = recipe, model = model)

}


#' @rdname train
#'
train.TunedRecipe <- function(x, model, ...) {

  last_trainbits <- model@trainbits
  if (is(model, "SelectedModel") || is(model, "TunedModel")) {
    model@params[names(x@params)] <- x@params
  } else {
    model <- do.call(SelectedModel, c(model, x@params))
  }
  grid <- x@grid
  x <- as(x, "ModelRecipe")

  if (any(dim(grid) == 0)) return(x)

  update_x <- list(update, x)

  trained_models <- list()
  grid_inds <- seq_len(nrow(grid))
  for (i in grid_inds) {
    x <- eval(as.call(c(update_x, grid[i, ])))
    trained_models[[i]] <- train(model, x)$model
  }

  trainbits <- do.call(c, lapply(trained_models, slot, name = "trainbits"))
  num_models <- nrow(trainbits@grid) / nrow(grid)
  trainbits@grid <- tibble(
    Model = trainbits@grid,
    Recipe = grid[rep(grid_inds, each = num_models), ]
  )

  selected <- ceiling(trainbits@selected / num_models)
  recipe <- eval(as.call(c(update_x, grid[selected, ])))
  model <- trained_models[[selected]]
  model@trainbits <- if (is.null(last_trainbits)) trainbits else last_trainbits

  list(x = recipe, model = model)

}
