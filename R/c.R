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
