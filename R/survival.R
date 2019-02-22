basesurv <- function(y, risk) {
  times <- y[, "time"]
  events <- pmin(y[, "status"], 1)
  n.event <- drop(rowsum(events, times))
  n.risk <- rowsum(risk, times) %>% rev %>% cumsum %>% rev
  structure(
    list(time = sort(unique(times)),
         n.event = n.event,
         surv = exp(-cumsum(n.event / n.risk))),
    class = c("BaseSurv", "survfit")
  )
}


mean.BaseSurv <- function(x, new_risk, ...) {
  times <- x$time[x$n.event > 0]
  surv_mean(times, predict(x, times, new_risk), max(x$time))
}


mean.survfit <- function(x, max_time = max(x$time), ...) {
  is_event <- x$n.event > 0
  surv_mean(x$time[is_event], x$surv[is_event], max_time)
}


predict.BaseSurv <- function(object, times, new_risk, ...) {
  basesurv <- NextMethod()
  t(outer(basesurv, new_risk, "^"))
}


predict.Surv <- function(object, x, ...) {
  .predict.Surv(object, x, ...)
}


.predict.Surv <- function(y, object, ...) {
  UseMethod(".predict.Surv", object)
}


.predict.Surv.list <- function(y, object, times, ...) {
  if (length(times)) {
    t(sapply(object, function(x) predict(x, times)))
  } else {
    max_time <- surv_max(y)
    sapply(object, function(x) mean(x, max_time = max_time))
  }
}


.predict.Surv.matrix <- function(y, object, times, ...) {
  fit <- structure(list(time = y[, "time"]), class = "survfit")
  if (length(times)) {
    t(apply(object, 1, function(x) {
        fit$n.event <- -diff(c(1, x))
        fit$surv <- x
        predict(fit, times)
      }))
  } else {
    apply(object, 1, function(x) {
      fit$n.event <- -diff(c(1, x))
      fit$surv <- x
      mean(fit)
    })
  }
}


.predict.Surv.numeric <- function(y, object, times, new_lp, ...) {
  risk <- exp(object)
  new_risk <- exp(new_lp)
  x <- basesurv(y, risk)
  if (length(times)) {
    predict(x, times, new_risk)
  } else {
    mean(x, new_risk)
  }
}


predict.survfit <- function(object, times, ...) {
  idx <- findInterval(times, object$time)
  c(1, object$surv)[idx + 1]
}


surv_max <- function(y) {
  max(y[, "time"])
}


surv_mean <- function(times, surv, max_time = max(times)) {
  times <- c(times, max_time)
  surv <- cbind(rbind(surv), 0)
  stopifnot(length(times) == ncol(surv))
  -as.numeric(times %*% diff(t(cbind(1, surv))))
}


surv_times <- function(y) {
  sort(unique(y[y[, "status"] != 0, "time"]))
}
