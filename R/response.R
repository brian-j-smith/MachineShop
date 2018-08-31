response <- function(object, ...) {
  UseMethod("response", object)
}


response.data.frame <- function(object, ...) {
  model.response(object)
}


response.formula <- function(object, data, ...) {
  eval(object[[2]], data)
}


response.C50Fit <- function(object, ...) {
  object$y
}


response.CForestFit <- function(object, ...) {
  object@responses@variables[[1]]
}


response.CoxFit <- function(object, ...) {
  object$y
}


response.GBMFit <- function(object, ...) {
  switch(object$distribution$name,
    "multinomial" = matrix(object$data$y, ncol = object$num.classes) %>%
      max.col %>%
      factor(levels = 1:object$num.classes, labels = object$classes),
    "coxph" = with(object$data, Surv(y, Misc)[order(i.timeorder),]),
    object$data$y
  )
}


response.GLMFit <- function(object, ...) {
  response(object$formula, object$data)
}


response.GLMNetFit <- function(object, ...) {
  response(object$mf)
}


response.NNetFit <- function(object, ...) {
  object$y
}


response.PLSFit <- function(object, ...) {
  object$y
}


response.POLRFit <- function(object, ...) {
  object$model[[1]]
}


response.RandomForestFit <- function(object, ...) {
  object$y
}


response.SVMFit <- function(object, ...) {
  if(is.character(object@lev)) {
    factor(object@ymatrix, levels = 1:object@nclass, labels = object@lev)
  } else {
    object@ymatrix
  }
}


response.SurvRegFit <- function(object, ...) {
  object$y
}
