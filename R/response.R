response <- function(object, ...) {
  UseMethod("response", object)
}


response.cph <- function(object, ...) {
  object$y
}


response.data.frame <- function(object, ...) {
  model.response(object)
}


response.formula <- function(object, data, ...) {
  eval(object[[2]], data)
}


response.gbm <- function(object, ...) {
  switch(object$distribution$name,
         "multinomial" = matrix(object$data$y, ncol = object$num.classes) %>%
           max.col %>%
           factor(levels = 1:object$num.classes, labels = object$classes),
         "coxph" = with(object$data, Surv(y, Misc)[order(i.timeorder),]),
         object$data$y
  )
}


response.glm <- function(object, ...) {
  response(object$formula, object$data)
}


response.ksvm <- function(object, ...) {
  if(is.character(object@lev)) {
    factor(object@ymatrix, levels = 1:object@nclass, labels = object@lev)
  } else {
    object@ymatrix
  }
}


response.randomForest <- function(object, ...) {
  object$y
}


response.RandomForest <- function(object, ...) {
  object@responses@variables[[1]]
}


response.survreg <- function(object, ...) {
  object$y
}
