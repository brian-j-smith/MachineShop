#' Combine MachineShop Objects
#'
#' Combine one or more \pkg{MachineShop} objects of the same class.
#'
#' @name combine
#' @rdname combine-methods
#'
#' @param ... named or unnamed \link{calibration}, \link{confusion},
#'   \link{lift}, \link[=curves]{performance curve}, \link{summary}, or
#'   \link{resample} results.  Curves must have been generated with the same
#'   performance \link{metrics} and resamples with the same resampling
#'   \link[=controls]{control}.
#' @param e1,e2 objects.
#'
#' @return Object of the same class as the arguments.
#'
NULL


c.BinomialVariate <- function(...) {
  args <- list(...)
  if (all(map_logi(is, args, "BinomialVariate"))) {
    structure(do.call(rbind, args), class = "BinomialVariate")
  } else {
    NextMethod()
  }
}


#' @rdname combine-methods
#'
c.Calibration <- function(...) {
  args <- list(...)
  if (all(map_logi(is, args, "Calibration"))) {

    if (!identical_elements(args, function(x) x@smoothed)) {
      msg <- "Calibration arguments are a mix of smoothed and binned curves"
      throw(Error(msg))
    }

    df <- do.call(append, set_model_names(args))
    Calibration(df, smoothed = args[[1]]@smoothed, .check = FALSE)

  } else {
    NextMethod()
  }
}


#' @rdname combine-methods
#'
c.ConfusionList <- function(...) {
  args <- list(...)
  is_valid <- function(x) is(x, "ConfusionList") || is(x, "ConfusionMatrix")
  if (all(map_logi(is_valid, args))) {

    conf_list <- list()
    for (i in seq_along(args)) {
      x <- args[[i]]
      if (is(x, "ConfusionMatrix")) x <- list("Model" = x)
      arg_name <- names(args)[i]
      if (!is.null(arg_name) && nzchar(arg_name)) {
        names(x) <- rep(arg_name, length(x))
      }
      conf_list <- c(conf_list, x)
    }
    names(conf_list) <- make.unique(names(conf_list))

    ConfusionList(conf_list)

  } else {
    NextMethod()
  }
}


#' @rdname combine-methods
#'
c.ConfusionMatrix <- function(...) {
  args <- list(...)
  args[[1]] <- ConfusionList(ListOf(args[1]))
  if (is.null(names(args)[1])) names(args)[1] <- "Model"
  do.call(c, args)
}


c.DiscreteVariate <- function(...) {
  args <- list(...)
  x <- NextMethod()
  class <- class(args[[1]])
  if (all(map_logi(is, args, class))) {
    new(class, x,
        min = min(map_num(slot, args, "min")),
        max = max(map_num(slot, args, "max")))
  } else {
    x
  }
}


#' @rdname combine-methods
#'
c.LiftCurve <- function(...) {
  NextMethod()
}


#' @rdname combine-methods
#'
c.ListOf <- function(...) {
  args <- list(...)
  class <- class1(args[[1]][[1]])
  is_valid <- function(x) {
    is(x, "ListOf") && is(x[[1]], class) && is(x[[1]], "vector")
  }
  if (all(map_logi(is_valid, args))) {
    x <- list()
    for (i in seq_along(args)) {
      name <- names(args)[i]
      if (!is.null(name) && nzchar(name)) {
        names(args[[i]]) <- rep(name, length(args[[i]]))
      }
      x <- c(x, args[[i]])
    }
    if (!is.null(names(x))) names(x) <- make.unique(names(x))
    ListOf(x)
  } else {
    NextMethod()
  }
}


c.Performance <- function(...) {
  args <- list(...)
  if (all(map_logi(is, args, "Performance"))) {
    if (length(args) > 1) {

      if (!identical_elements(args, function(x) dimnames(x)[1:2])) {
        throw(Error("Performance objects have different row or column names"))
      }

      if (!identical_elements(args, function(x) x@control)) {
        throw(Error("Performance arguments have different control structures"))
      }

      Performance(abind(args, along = 3), control = args[[1]]@control)

    } else {
      args[[1]]
    }
  } else {
    NextMethod()
  }
}


#' @rdname combine-methods
#'
c.PerformanceCurve <- function(...) {
  args <- list(...)
  class <- class(args[[1]])
  if (all(map_logi(is, args, class))) {

    if (!identical_elements(args, function(x) x@metrics)) {
      throw(Error(class, " arguments have different metrics"))
    }

    df <- do.call(append, set_model_names(args))
    do.call(class, list(df, metrics = args[[1]]@metrics, .check = FALSE))

  } else {
    NextMethod()
  }
}


#' @rdname combine-methods
#'
c.Resamples <- function(...) {
  args <- list(...)
  if (all(map_logi(is, args, "Resamples"))) {

    if (!identical_elements(args, function(x) x@control)) {
      throw(Error("Resamples arguments have different control structures"))
    }

    if (!identical_elements(args, function(x) x@strata)) {
      throw(Error("Resamples arguments have different strata variables"))
    }

    df <- do.call(append, set_model_names(args))
    Resamples(df, control = args[[1]]@control, strata = args[[1]]@strata,
              .check = FALSE)

  } else {
    NextMethod()
  }
}


c.SurvMatrix <- function(...) {
  args <- list(...)
  arg1 <- args[[1]]
  class <- class(arg1)
  if (all(map_logi(is, args, class))) {

    if (!identical_elements(args, function(x) x@times)) {
      throw(Error(class, " arguments have different times"))
    }

    if (!identical_elements(args, function(x) x@distr)) {
      throw(Error(class, " arguments have different distributions"))
    }

    new(class, do.call(rbind, args), times = arg1@times, distr = arg1@distr)

  } else {
    NextMethod()
  }
}


c.SurvMeans <- function(...) {
  args <- list(...)
  x <- NextMethod()
  class <- class(args[[1]])
  if (all(map_logi(is, args, class))) {
    new(class, x, distr = args[[1]]@distr)
  } else {
    x
  }
}


#' @rdname combine-methods
#'
setMethod("+", c("SurvMatrix", "SurvMatrix"),
  function(e1, e2) {
    x <- callNextMethod()
    class <- class(e1)
    if (all(class(e2) == class, e1@times == e2@times, e1@distr == e2@distr)) {
      new(class, x, times = e1@times, distr = e1@distr)
    } else x
  }
)
