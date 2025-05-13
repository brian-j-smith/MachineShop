GridSearch <- function(...) {
  new("GridSearch", ...,
    label = "Grid Search",
    fun = function(optim, grid, ...) {
      max_iter <- nrow(grid)
      for (i in seq_len(max_iter)) {
        optim(grid[i, ], max_iter = max_iter)
      }
      return(NULL)
    }
  ) %>% set_monitor
}


NullOptimization <- function() {
  new("NullOptimization", label = "Null Optimization")
}


RandomGridSearch <- function(..., size = numeric()) {
  size <- check_integer(size, bounds = c(1, Inf), size = 1)
  throw(check_assignment(size))

  new("RandomGridSearch",
    GridSearch(...), label = "Random Grid Search", size = size
  )
}


SequentialOptimization <- function(..., random = FALSE) {
  if (!isFALSE(random)) {
    random <- check_integer(random, bounds = c(1, Inf), size = 1)
    throw(check_assignment(random))
  }

  new("SequentialOptimization", ..., random = random) %>% set_monitor
}


optim <- function(.fun, object, ...) {

  method <- get_optim_field(object, "label")
  progress <- if (get_optim_field(object, "monitor")$progress) {
    heading(paste(class(object), "optimization"))
    new_print_progress()
  } else {
    function(...) NULL
  }

  grid <- get_grid(object, ...)
  optim_grid <- unnest_params(grid$params)
  names(optim_grid) <- make_names_len(length(optim_grid), "param")
  optim_params <- as.list(optim_grid[1, ])
  optim_params[] <- list(NULL)

  optim_bounds <- map(function(param) {
    if (all(is.finite(param)) && length(unique(param)) > 1) range(param)
  }, optim_grid)
  optim_bounds <- optim_bounds[!map("logi", is.null, optim_bounds)]

  control <- object@params@control
  metrics <- object@params@metrics
  cutoff <- object@params@cutoff
  stat_fun <- function(x) object@params@stat(na.omit(x))

  err_msgs <- character()
  model_params_list <- list()
  perf <- NULL
  perf_list <- list()
  perf_stats_list <- list()
  scores <- numeric()

  optim_fun <- function(x, max_iter = Inf) {
    optim_params[names(x)] <- x
    model_params <- renest_params(optim_params, grid$params[1, ])
    model_params_list <<- c(model_params_list, list(model_params))

    perf_stats <- NA
    metric <- NULL
    score <- tryCatch(
      {
        res <- resample(
          update(object, params = model_params), ..., control = control,
          progress_index = new_progress_index(
            length(scores) + 1, max = max_iter, name = class(object)
          )
        )
        err_msgs <<- c(err_msgs, NA)
        if (is.null(metrics)) {
          metrics <<- get_perf_metrics(res$Observed, res$Predicted)
        }
        perf <<- performance(res, metrics = metrics, cutoff = cutoff)
        perf_list <<- c(perf_list, list(perf))
        perf_stats <- apply(perf, 2, stat_fun)
        perf_stats_list <<- c(perf_stats_list, list(perf_stats))
        metric <- as.MLMetric(c(metrics)[[1]])
        ifelse(metric@maximize, 1, -1) * perf_stats[1]
      },
      error = function(cond) {
        err_msgs <<- c(err_msgs, conditionMessage(cond))
        perf_list <<- c(perf_list, list(NA))
        perf_stats_list <<- c(perf_stats_list, list(perf_stats))
        -Inf
      }
    )
    scores <<- c(scores, score)

    progress(
      scores, max_iter = max_iter, method = method,
      items = list(
        "Parameter{?s}: " = unlist(
          unnest_params(model_params, compact_names = TRUE)
        )
      ),
      metric = structure(perf_stats[1], names = getElement(metric, "name"))
    )

    score
  }

  optim_res <- .fun(
    optim = optim_fun,
    grid = optim_grid,
    bounds = optim_bounds,
    params = get_optim_field(object, "params"),
    monitor = get_optim_field(object, "monitor")
  )

  names(perf_list) <- if (is_optim_method(object, "GridSearch")) {
    grid$name
  } else {
    grid_name <- sub("[.].*$", "", grid$name[1])
    make_unique(rep_len(grid_name, length(scores)))
  }

  failed <- is.na(perf_list)
  names(err_msgs) <- names(perf_list)
  err_msgs <- na.omit(err_msgs)
  err_msgs <- paste0(names(err_msgs), ": ", err_msgs, collapse = "\n")
  if (all(failed)) {
    throw(LocalError("Resampling failed for all models.\n", err_msgs))
  } else if (any(failed)) {
    throw(LocalWarning("Resampling failed for some models.\n", err_msgs))
    perf[] <- NA
    perf_list[failed] <- list(perf)
    perf_stats_list[failed] <- list(perf[1, ])
  }

  step <- TrainingStep(
    object,
    method = method,
    names = names(perf_list),
    items = optim_res,
    params = do.call(rbind, model_params_list),
    metrics = do.call(rbind, perf_stats_list),
    selected = which.max(scores),
    performance = do.call(c, perf_list)
  )

  model_params <- step@log$params[step@log$selected, ]
  push(step, fit(update(object, params = model_params), ...))

}
