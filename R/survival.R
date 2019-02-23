basesurv <- function(y, risk, method = c("efron", "breslow",
                                         "fleming-harrington"), ...) {
  times <- y[, "time"]
  events <- pmin(y[, "status"], 1)
  surv <- switch(match.arg(method),
                 "breslow" = basesurv_breslow,
                 "efron" = basesurv_efron,
                 "fleming-harrington" = basesurv_fh)
  fit <- surv(times, events, risk)
  structure(
    list(n = length(y), time = sort(unique(times)),
         n.risk = fit$n.risk, n.event = fit$n.event,
         n.censor = fit$n.total - fit$n.event,
         surv = fit$surv),
    class = c("BaseSurv", "survfit")
  )
}


basesurv_breslow <- function(times, events, risk) {
  n <- unname(rowsum(cbind(1, events, risk), times))
  n.event <- n[, 2]
  n.risk <- cumsum_risk(n[, 3])
  hazard <- n.event / n.risk
  list(n.total = n[, 1], n.event = n.event, n.risk = n.risk,
       surv = exp(-cumsum(hazard)))
}


basesurv_efron <- function(times, events, risk) {
  f <- function(n.event, n.risk_all, n.risk_events) {
    if (n.event) {
      n.event / prod(n.risk_all - (1:n.event - 1) / n.event * n.risk_events)
    } else 0
  }
  basesurv_function(times, events, risk, f)
}


basesurv_fh <- function(times, events, risk) {
  f <- function(n.event, n.risk_all, n.risk_events) {
    if (n.event) {
      sum(1 / (n.risk_all - (1:n.event - 1) / n.event * n.risk_events))
    } else 0
  }
  basesurv_function(times, events, risk, f)
}


basesurv_function <- function(times, events, risk, f) {
  n <- unname(rowsum(cbind(1, events, risk, risk * events), times))
  n.event <- n[, 2]
  n.risk_all <- cumsum_risk(n[, 3])
  n.risk_events <- cumsum_risk(n[, 4])
  hazard <- mapply(f, n.event, n.risk_all, n.risk_events)
  list(n.total = n[, 1], n.event = n.event, n.risk = n.risk_all,
       surv = exp(-cumsum(hazard)))
}


cumsum_risk <- function(x) rev(cumsum(rev(x)))


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
  x <- basesurv(y, risk, ...)
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
