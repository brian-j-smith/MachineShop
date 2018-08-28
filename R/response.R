response <- function(object, ...) {
  UseMethod("response", object)
}


response.coxph <- function(object, ...) {
  object$y
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


response.randomForest <- function(object, ...) {
  object$y
}


response.RandomForest <- function(object, ...) {
  object@responses@variables[[1]]
}


response.survreg <- function(object, ...) {
  object$y
}
