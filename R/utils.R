basehaz <- function(y, risk, times) {
  y_times <- unique(y[,"time"]) %>% sort
  nrisk <- rowsum(risk, y[,"time"]) %>% rev %>% cumsum %>% rev
  nevent <- rowsum(y[,"status"], y[,"time"])[,1]
  cumhaz <- cumsum(nevent / nrisk) %>% structure(names = NULL)
  idx <- approx(y_times, seq(y_times), times, method = "constant",
                f = 0, yleft = 0, yright = length(y_times))$y
  c(0, cumhaz)[idx + 1]
}


probs.survfit <- function(object, times) {
  survtimes <- c(0, object$time)
  survprobs <- c(1, object$surv)
  idx <- sapply(times, function(x) max(which(survtimes <= x)))
  survprobs[idx]
}


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
