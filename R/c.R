#' Combine MachineShop Objects
#' 
#' Combine one or more \pkg{MachineShop} objects of the same class.
#' 
#' @name c
#' @aliases combine
#' @rdname c
#' 
#' @param ... named or unnamed \link{resample} results.  Resamples must have
#' been generated with the same resampling \link[=controls]{control}.
#' 
#' @return Object of the same class as the arguments.
#' 
NULL


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


c.MLTune <- function(...) {
  args <- list(...)
  if (all(mapply(is, args, "MLTune"))) {
    if (length(args) > 1) {
      
      grid <- do.call(append, lapply(args, slot, name = "grid"))
      
      values_list <- lapply(args, slot, name = "values")
      values <- unlist(values_list)
      names(values) <- paste0(names(values), ".",
                              rep(seq(args), sapply(values_list, length)))
      
      performance <- do.call(Performance,
                             lapply(args, slot, name = "performance"))
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
