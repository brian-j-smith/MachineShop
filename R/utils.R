create <- function(class, ...) {
  if(is.character(class) || isClassDef(class)) {
    new(class, ...)
  } else {
    class$new(...)
  }
}


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
