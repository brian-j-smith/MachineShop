basehaz <- function(y, risk, times) {
  y_times <- unique(y[, "time"]) %>% sort
  nrisk <- rowsum(risk, y[, "time"]) %>% rev %>% cumsum %>% rev
  nevent <- rowsum(y[, "status"], y[, "time"])[, 1]
  cumhaz <- cumsum(nevent / nrisk) %>% unname
  idx <- approx(y_times, seq_along(y_times), times, method = "constant",
                f = 0, yleft = 0, yright = length(y_times))$y
  c(0, cumhaz)[idx + 1]
}


surv_max <- function(x) {
  max(x[, "time"])
}


surv_mean <- function(times, probs, max_time = max(times)) {
  times <- c(times, max_time)
  probs <- cbind(probs, 0)
  stopifnot(length(times) == ncol(probs))
  -drop(times %*% diff(t(cbind(1, probs))))
}


surv_times <- function(x) {
  sort(unique(x[x[, "status"] == 1, "time"]))
}
