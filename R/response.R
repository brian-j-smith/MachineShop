response <- function(object, ...) {
  UseMethod("response", object)
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
