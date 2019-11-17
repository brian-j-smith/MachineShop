#' Combine MachineShop Objects
#' 
#' Combine one or more \pkg{MachineShop} objects of the same class.
#' 
#' @name c
#' @aliases combine
#' @rdname c
#' 
#' @param ... named or unnamed \link{calibration}, \link{confusion},
#' \link[=performance_curve]{performance curve}, \link{lift}, or \link{resample}
#' results.  Curves must have been generated with the same performance
#' \link{metrics} and resamples with the same resampling
#' \link[=controls]{control}.
#' 
#' @return Object of the same class as the arguments.
#' 
NULL


#' @rdname c
#' 
c.Calibration <- function(...) {
  args <- list(...)
  if (all(mapply(is, args, "Calibration"))) {
    
    smoothed <- args[[1]]@smoothed
    if (!all(sapply(args, function(x) identical(x@smoothed, smoothed)))) {
      stop("Calibration arguments are a mix of smoothed and binned curves")
    }
    
    df <- do.call(append, make_unique_levels(args, which = "Model"))
    rownames(df) <- NULL
    Calibration(df, smoothed = smoothed)
    
  } else {
    NextMethod()
  }
}


#' @rdname c
#' 
c.Confusion <- function(...) {
  args <- list(...)
  is_confusion <- function(x) is(x, "Confusion") || is(x, "ConfusionMatrix")
  if (all(sapply(args, is_confusion))) {
    
    conf_list <- list()
    for (i in seq(args)) {
      x <- args[[i]]
      if (is(x, "ConfusionMatrix")) x <- list("Model" = x)
      arg_name <- names(args)[i]
      if (!is.null(arg_name) && nzchar(arg_name)) {
        names(x) <- rep(arg_name, length(x))
      }
      conf_list <- c(conf_list, x)
    }
    names(conf_list) <- make.unique(names(conf_list))
    
    structure(conf_list, class = c("Confusion", "listof"))
    
  } else {
    NextMethod()
  }
}


#' @rdname c
#' 
c.ConfusionMatrix <- function(...) {
  c.Confusion(...)
}


#' @rdname c
#' 
c.Curves <- function(...) {
  args <- list(...)
  class <- class(args[[1]])
  if (all(mapply(is, args, class))) {
    
    metrics <- args[[1]]@metrics
    if (!all(sapply(args, function(x) identical(x@metrics, metrics)))) {
      stop(class, " arguments have different metrics")
    }
    
    df <- do.call(append, make_unique_levels(args, which = "Model"))
    rownames(df) <- NULL
    do.call(class, list(df, metrics = metrics))
    
  } else {
    NextMethod()
  }
}


c.DiscreteVector <- function(...) {
  args <- list(...)
  x <- NextMethod()
  class <- class(args[[1]])
  if (all(mapply(is, args, class))) {
    new(class, x,
        min = min(sapply(args, slot, name = "min")),
        max = max(sapply(args, slot, name = "max")))
  } else {
    x
  }
}


#' @rdname c
#' 
c.Lift <- function(...) {
  NextMethod()
}


c.MLTune <- function(...) {
  args <- list(...)
  if (all(mapply(is, args, "MLTune"))) {
    if (length(args) > 1) {
      
      grid <- do.call(append, lapply(args, slot, name = "grid"))
      
      values_list <- lapply(args, slot, name = "values")
      values <- unlist(values_list)
      names(values) <- paste0(names(values), ".",
                              rep(seq(args), sapply(values_list, length)))
      
      performance <- do.call(c, lapply(args, slot, name = "performance"))
      dimnames(performance)[[3]] <- names(values)
      
      metric <- args[[1]]@metric
      if (!all(sapply(args, function(x) identical(x@metric, metric)))) {
        stop("MLTune objects have different metric functions")
      }
      
      selected <- ifelse(metric@maximize, which.max, which.min)(values)
      selected_names <- names(sapply(args, slot, name = "selected"))
      if (!all(selected_names == selected_names[1])) {
        stop("MLTune objects have difference selected metric names")
      }
      names(selected) <- selected_names[1]
      
      MLTune(grid = grid, performance = performance, selected = selected,
             values = values, metric = metric)
      
    } else {
      args[[1]]
    }
  } else {
    NextMethod()
  }
}


c.Performance <- function(...) {
  args <- list(...)
  if (all(mapply(is, args, "Performance"))) {
    if (length(args) > 1) {
      
      names <- dimnames(args[[1]])[1:2]
      if (!all(sapply(args, function(x) identical(dimnames(x)[1:2], names)))) {
        stop("Performance objects have different row or column names")
      }
      
      Performance(abind(args, along = 3))
      
    } else {
      args[[1]]
    }
  } else {
    NextMethod()
  }
}


#' @rdname c
#' 
c.Resamples <- function(...) {
  args <- list(...)
  if (all(mapply(is, args, "Resamples"))) {
    
    control <- args[[1]]@control
    if (!all(sapply(args, function(x) identical(x@control, control)))) {
      stop("Resamples arguments have different control structures")
    }
    
    strata <- args[[1]]@strata
    if (!all(sapply(args, function(x) identical(x@strata, strata)))) {
      stop("Resamples arguments have different strata variables")
    }
    
    df <- do.call(append, make_unique_levels(args, which = "Model"))
    rownames(df) <- NULL
    Resamples(df, control = control, strata = strata)
    
  } else {
    NextMethod()
  }
}
