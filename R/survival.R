#################### Surv Prediction Method ####################


predict.Surv <- function(
  object, ..., times = numeric(), distr = character(), weights = NULL
) {
  distr <- if (is_counting(object)) {
    "empirical"
  } else if (is_empty(distr)) {
    settings(if (length(times)) "distr.SurvProbs" else "distr.SurvMeans")
  } else {
    match.arg(distr, c("empirical", "exponential", "rayleigh", "weibull"))
  }
  SurvPrediction(
    .predict.Surv(object, ..., times = times, distr = distr, weights = weights),
    times = times, distr = distr
  )
}


.predict.Surv <- function(object, ...) {
  UseMethod(".predict.Surv", ..1)
}


.predict.Surv.list <- function(object, x, times, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- predict(object, x[[i]], times = times, ...)
  }
  if (length(times)) do.call(rbind, x) else as.numeric(x)
}


.predict.Surv.matrix <- function(object, x, times, distr, ...) {
  individual_fits <- surv_fit(distr, SurvProbs(x, time(object)))
  if (length(times)) {
    predict(individual_fits, times = times)
  } else {
    mean(individual_fits)
  }
}


.predict.Surv.numeric <- function(object, lp, new_lp, times, distr, ...) {
  risks <- exp(lp)
  new_risks <- exp(new_lp)
  baseline_fit <- surv_fit(distr, object, risks = risks, ...)
  if (length(times)) {
    predict(baseline_fit, times = times, new_risks = new_risks)
  } else {
    mean(baseline_fit, new_risks = new_risks)
  }
}


.predict.Surv.survfit <- function(object, x, times, distr, ...) {
  individual_fit <- surv_fit(distr, x)
  if (length(times)) {
    predict(individual_fit, times = times)
  } else {
    mean(individual_fit, max_time = max(time(object)))
  }
}


#################### Empirical Survival Distribution ####################


EmpiricalSurv <- function(x, ...) {
  UseMethod("EmpiricalSurv")
}


EmpiricalSurv.Surv <- function(
  x, risks = numeric(), weights = NULL, method = c("efron", "breslow"), ...
) {
  event <- as.integer(x[, "status"])
  if (is_empty(risks)) risks <- 1
  weights <- check_weights(weights, x)
  throw(check_assignment(weights))
  if (is_empty(method)) method <- settings("method.EmpiricalSurv")
  method <- match.arg(method)

  data <- data.frame(wt = weights, wt_censor = weights * !event,
                     wt_event = weights * event, wt_risk = weights * risks)
  sums <- cbind(
    rowsum(data[c("wt_censor", "wt_event")], time(x)),
    risksum(data[c("wt", "wt_risk")], x)
  )

  cumhaz <- cumsum(switch(method,
    "breslow" = sums$wt_event / sums$wt_risk,
    "efron" = {
      data <- data.frame(event, wt_eventrisk = data$wt_event * risks)
      sums[names(data)] <- rowsum(data, time(x))
      hazfit_efron(sums$event, sums$wt_event, sums$wt_risk, sums$wt_eventrisk)
    }
  ))

  structure(
    list(n = length(x),
         time = sums$stop_time,
         n.risk = sums$wt,
         n.event = sums$wt_event,
         n.censor = sums$wt_censor,
         surv = exp(-cumhaz),
         cumhaz = cumhaz),
    class = c("EmpiricalSurv", "survfitcox", "survfit")
  )
}


EmpiricalSurv.survfit <- function(x, ...) {
  if (!is(x, "EmpiricalSurv")) class(x) <- c("EmpiricalSurv", class(x))
  x
}


EmpiricalSurv.SurvProbs <- function(x, ...) {
  x
}


mean.EmpiricalSurv <- function(x, new_risks = numeric(), ...) {
  times <- x$time[x$n.event > 0]
  surv <- predict(x, times = times, new_risks = new_risks)
  surv_mean(times, surv, max(x$time))
}


predict.EmpiricalSurv <- function(object, times, new_risks = numeric(), ...) {
  surv <- NextMethod()
  if (length(new_risks)) t(outer(surv, new_risks, "^")) else rbind(surv)
}


#################### Exponential Distribution ####################


Exponential <- function(...) {
  Weibull(..., shape = 1)
}


#################### Rayleigh Distribution ####################


Rayleigh <- function(...) {
  Weibull(..., shape = 2)
}


#################### Weibull Distribution ####################


Weibull <- function(x, ...) {
  UseMethod("Weibull")
}


Weibull.numeric <- function(x = scale, shape, scale, ...) {
  structure(list(shape = shape, scale = x), class = "Weibull")
}


Weibull.Surv <- function(
  x, risks = numeric(), shape = numeric(), weights = NULL, ...
) {
  if (length(shape)) {
    nparams <- 1
  } else {
    shape <- Inf
    nparams <- 2
  }
  params <- if (length(event_time(x)) >= nparams) {
    fo <- if (length(risks)) { x ~ offset(-log(risks)) } else { x ~ 1 }
    regfit <- survreg(fo, dist = "weibull", scale = 1 / shape,
                      weights = weights)
    c(1 / regfit$scale, exp(coef(regfit)[[1]]))
  } else c(NA_real_, NA_real_)
  Weibull(shape = params[1], scale = params[2]^-params[1])
}


Weibull.survfit <- function(x, weights = NULL, ...) {
  weights <- x$n / sum(x$n.event + x$n.censor)
  time_event <- rep(x$time, round(weights * x$n.event))
  time_censor <- rep(x$time, round(weights * x$n.censor))
  time <- c(time_event, time_censor)
  status <- rep(c(1, 0), c(length(time_event), length(time_censor)))
  Weibull(Surv(time, status), ...)
}


Weibull.SurvProbs <- function(x, shape = numeric(), ...) {
  weibullfit <- if (length(shape)) {
    function(df) c(mean(df$y - shape * df$x), shape)
  } else {
    function(df) if (nrow(df) >= 2) {
      coef(lm(y ~ x, data = df))
    } else c(NA_real_, NA_real_)
  }
  coef <- apply(x, 1, function(surv) {
    df <- surv_cases(x = log(x@times), y = log(-log(surv)),
                     subset = diff(c(1, surv)) < 0)
    weibullfit(df)
  })
  Weibull(shape = coef[2, ], scale = exp(coef[1, ]))
}


mean.Weibull <- function(x, new_risks = numeric(), ...) {
  if (length(new_risks)) x$scale <- new_risks * x$scale
  x$scale^(-1 / x$shape) * gamma(1 + 1 / x$shape)
}


predict.Weibull <- function(object, times, new_risks = numeric(), ...) {
  if (is_empty(new_risks)) new_risks <- 1
  shape <- object$shape
  times_shape <- if (length(shape) == 1) {
    matrix(times^shape, length(new_risks), length(times), byrow = TRUE)
  } else {
    t(outer(times, shape, "^"))
  }
  exp((new_risks * -object$scale) * times_shape)
}


#################### survfit Methods ####################


mean.survfit <- function(x, max_time = max(x$time), ...) {
  is_event <- x$n.event > 0
  surv_mean(x$time[is_event], x$surv[is_event], max_time)
}


predict.survfit <- function(object, times, ...) {
  inds <- findInterval(times, object$time)
  c(1, object$surv)[inds + 1]
}


#################### SurvMatrix Methods ####################


mean.SurvProbs <- function(x, ...) {
  apply(x, 1, function(surv) surv_mean(x@times, surv))
}


predict.SurvProbs <- function(object, times, ...) {
  inds <- findInterval(times, object@times)
  cbind(1, object)[, inds + 1, drop = FALSE]
}


#################### Survival Utility Functions ####################


event_time <- function(x) {
  x <- x[x[, "status"] == 1]
  if (length(x)) {
    sort(unique(time(x)), na.last = TRUE, method = "quick")
  } else numeric()
}


get_surv_distr <- function(distr, observed, predicted) {
  if (is_counting(observed)) {
    "empirical"
  } else {
    if (is(predicted, "SurvProbs")) {
      pred_distr <- predicted@distr
      default_distr <- settings("distr.SurvProbs")
    } else {
      pred_distr <- if (is(predicted, "SurvTimes")) predicted@distr
      default_distr <- settings("distr.SurvMeans")
    }
    match.arg(
      na.omit(c(distr, pred_distr, default_distr))[1],
      c("empirical", names(survreg.distributions))
    )
  }
}


hazfit_efron <- function(event, wt_event, wt_risk, wt_eventrisk) {
  .Call(C_hazfit_efron, as.integer(event), as.double(wt_event),
        as.double(wt_risk), as.double(wt_eventrisk))
}


risksum <- function(x, group) {
  rcumsum <- function(x) rev(cumsum(rev(x)))
  stop <- time(group)
  stop_time <- sort(unique(stop), na.last = TRUE, method = "quick")
  res <- if (is_counting(group)) {
    start <- group[, "start"]
    start_time <- sort(unique(start), na.last = TRUE, method = "quick")
    unobserved <- approx(start_time, seq_along(start_time), stop_time,
                         method = "constant", f = 1,
                         yright = length(start_time) + 1)$y
    map(function(num_stop, num_start) {
      rcumsum(num_stop) - c(rcumsum(num_start), 0)[unobserved]
    }, rowsum(x, stop), rowsum(x, start))
  } else {
    map(function(num) rcumsum(num), rowsum(x, stop))
  }
  cbind(as.data.frame(res), stop_time = stop_time)
}


surv_cases <- function(..., subset = TRUE) {
  df <- data.frame(...)
  is_finite <- Reduce("&", map(is.finite, df))
  df[subset & is_finite, , drop = FALSE]
}


surv_fit <- function(x, ...) {
  f <- switch(x,
    "empirical" = EmpiricalSurv,
    "exponential" = Exponential,
    "rayleigh" = Rayleigh,
    "weibull" = Weibull
  )
  f(...)
}


surv_mean <- function(times, surv, max_time = max(times)) {
  times <- c(times, max_time)
  surv <- cbind(rbind(surv), 0)
  stopifnot(length(times) == ncol(surv))
  -as.numeric(times %*% diff(t(cbind(1, surv))))
}


surv_subset <- function(x, weights, include, time) {
  surv <- 1
  p <- weighted_mean(include, weights)
  x <- x[include]
  weights <- weights[include]
  if (length(x) && any(event_time(x) <= time)) {
    data <- data.frame(event = weights * x[, "status"], total = weights)
    sums <- cbind(
      rowsum(data["event"], time(x)),
      risksum(data["total"], x)
    )
    sums <- sums[sums$stop_time <= time, ]
    surv <- prod(1 - sums$event / sums$total)
  }
  list(surv = surv, p = p)
}


survmetric_mean <- function(x, times) {
  if (length(x) > 1) {
    weights <- diff(c(0, times)) / tail(times, 1)
    c("Mean" = sum(weights * x), x)
  } else {
    unname(x)
  }
}


time.MLModelFit <- function(x, ...) {
  NULL
}


time.StackedModelFit <- function(x, ...) {
  x$times
}


time.Surv <- function(x, ...) {
  throw(check_censoring(x, c("right", "counting")))
  x[, ncol(x) - 1]
}
