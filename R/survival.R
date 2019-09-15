#################### Surv Prediction Method ####################


predict.Surv <- function(object, x, ...) {
  .predict.Surv(object, x, ...)
}


.predict.Surv <- function(y, object, ...) {
  UseMethod(".predict.Surv", object)
}


.predict.Surv.list <- function(y, object, times, dist, ...) {
  if (length(times)) {
    dist <- surv_dist_probs(dist)
    t(sapply(object, function(x) predict(dist(x), times)))
  } else {
    dist <- surv_dist_mean(dist)
    max_time <- surv_max(y)
    sapply(object, function(x) mean(dist(x), max_time = max_time))
  }
}


.predict.Surv.matrix <- function(y, object, times, dist, ...) {
  x <- SurvProbs(object, y[, "time"])
  if (length(times)) {
    predict(surv_dist_probs(dist)(x), times)
  } else {
    mean(surv_dist_mean(dist)(x))
  }
}


.predict.Surv.numeric <- function(y, object, times, new_lp, dist, ...) {
  risk <- exp(object)
  new_risk <- exp(new_lp)
  if (length(times)) {
    predict(surv_dist_probs(dist)(y, risk, ...), times, new_risk)
  } else {
    mean(surv_dist_mean(dist)(y, risk, ...), new_risk)
  }
}


#################### Empirical Survival Distribution ####################


EmpiricalSurv <- function(x, ...) {
  UseMethod("EmpiricalSurv")
}


EmpiricalSurv.default <- function(x, ...) {
  x
}


EmpiricalSurv.Surv <- function(y, risk = NULL, method =
                                 c("breslow", "efron", "fleming-harrington"),
                               ...) {
  times <- y[, "time"]
  events <- pmin(y[, "status"], 1)
  if (is.null(risk)) risk <- rep(1, length(times))
  if (is.null(method)) method <- MachineShop::settings("method.EmpiricalSurv")
  surv <- switch(match.arg(method),
                 "breslow" = empiricalsurv_breslow,
                 "efron" = empiricalsurv_efron,
                 "fleming-harrington" = empiricalsurv_fh)
  fit <- surv(times, events, risk)
  structure(
    list(n = length(y), time = sort(unique(times)),
         n.risk = fit$n.risk, n.event = fit$n.event,
         n.censor = fit$n.total - fit$n.event,
         surv = fit$surv),
    class = c("EmpiricalSurv", "survfit")
  )
}


empiricalsurv_breslow <- function(times, events, risk) {
  n <- unname(rowsum(cbind(1, events, risk), times))
  n.event <- n[, 2]
  n.risk <- cumsum_risk(n[, 3])
  hazard <- n.event / n.risk
  list(n.total = n[, 1], n.event = n.event, n.risk = n.risk,
       surv = exp(-cumsum(hazard)))
}


empiricalsurv_efron <- function(times, events, risk) {
  f <- function(n.event, n.risk_all, n.risk_events) {
    if (n.event) {
      n.event / prod(n.risk_all - (1:n.event - 1) / n.event * n.risk_events)
    } else 0
  }
  empiricalsurv_function(times, events, risk, f)
}


empiricalsurv_fh <- function(times, events, risk) {
  f <- function(n.event, n.risk_all, n.risk_events) {
    if (n.event) {
      sum(1 / (n.risk_all - (1:n.event - 1) / n.event * n.risk_events))
    } else 0
  }
  empiricalsurv_function(times, events, risk, f)
}


empiricalsurv_function <- function(times, events, risk, f) {
  n <- unname(rowsum(cbind(1, events, risk, risk * events), times))
  n.event <- n[, 2]
  n.risk_all <- cumsum_risk(n[, 3])
  n.risk_events <- cumsum_risk(n[, 4])
  hazard <- mapply(f, n.event, n.risk_all, n.risk_events)
  list(n.total = n[, 1], n.event = n.event, n.risk = n.risk_all,
       surv = exp(-cumsum(hazard)))
}


mean.EmpiricalSurv <- function(x, new_risk = NULL, ...) {
  times <- x$time[x$n.event > 0]
  surv_mean(times, predict(x, times, new_risk), max(x$time))
}


predict.EmpiricalSurv <- function(object, times, new_risk = NULL, ...) {
  surv <- NextMethod()
  rbind(if (is.null(new_risk)) surv else sapply(surv, function(x) x^new_risk))
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


Weibull.Surv <- function(x, risk = NULL, shape = NULL, ...) {
  if (is.null(shape)) {
    shape <- Inf
    nparams <- 2
  } else {
    nparams <- 1
  }
  params <- if (length(surv_times(x)) >= nparams) {
    fo <- if (is.null(risk)) x ~ 1 else x ~ offset(-log(risk))
    regfit <- survreg(fo, dist = "weibull", scale = 1 / shape)
    c(1 / regfit$scale, exp(coef(regfit)[[1]]))
  } else c(NA_real_, NA_real_)
  Weibull(shape = params[1], scale = params[2]^-params[1])
}


Weibull.survfit <- function(x, ...) {
  time_event <- rep(x$time, times = x$n.event)
  time_censor <- rep(x$time, times = x$n.censor)
  time <- c(time_event, time_censor)
  status <- rep(c(1, 0), times = c(length(time_event), length(time_censor)))
  Weibull(Surv(time, status), ...)
}


Weibull.SurvProbs <- function(x, shape = NULL, ...) {
  weibullfit <- if (is.null(shape)) {
    function(df) if (nrow(df) >= 2) {
      coef(lm(y ~ x, data = df))
    } else c(NA_real_, NA_real_)
  } else {
    function(df) c(mean(df$y - shape * df$x), shape)
  }
  coef <- apply(x, 1, function(surv) {
    df <- surv_cases(x = log(time(x)), y = log(-log(surv)),
                     subset = diff(c(1, surv)) < 0)
    weibullfit(df)
  })
  Weibull(shape = coef[2, ], scale = exp(coef[1, ]))
}


mean.Weibull <- function(x, new_risk = NULL, ...) {
  if (!is.null(new_risk)) x$scale <- new_risk * x$scale
  x$scale^(-1 / x$shape) * gamma(1 + 1 / x$shape)
}


predict.Weibull <- function(object, times, new_risk = NULL, ...) {
  if (is.null(new_risk)) new_risk <- 1
  shape <- object$shape
  times_shape <- if (length(shape) == 1) {
    matrix(times^shape, length(new_risk), length(times), byrow = TRUE)
  } else {
    sapply(times, function(time) time^shape)
  }
  exp((new_risk * -object$scale) * times_shape)
}


#################### survfit Methods ####################


mean.survfit <- function(x, max_time = max(x$time), ...) {
  is_event <- x$n.event > 0
  surv_mean(x$time[is_event], x$surv[is_event], max_time)
}


predict.survfit <- function(object, times, ...) {
  idx <- findInterval(times, object$time)
  c(1, object$surv)[idx + 1]
}


#################### SurvMatrix Constructors and Methods ####################


SurvMatrix <- function(object, times = NULL) {
  object <- as.matrix(object)
  
  if (is.null(times)) times <- rep(NA_real_, ncol(object))
  
  if (length(times) != ncol(object)) {
    stop("unequal number of survival times and predictions")
  }
  
  dimnames(object) <- list(NULL, paste("Time", seq(ncol(object))))
  
  structure(object, class = "SurvMatrix", times = times)
}


#' SurvMatrix Class Constructor
#' 
#' Create an object of predicted survival events or probabilites for use with
#' metrics provided by the \pkg{MachineShop} package.
#' 
#' @name SurvMatrix
#' @rdname SurvMatrix
#' 
#' @param object matrix, or object that can be converted to one, of predicted
#' survival events or probabilities with columns and rows representing
#' prediction times and cases, respectively.
#' @param times numeric vector of the survival prediction times.
#' 
#' @return Object that is of the same class as the constructor name and inherits
#' from \code{SurvMatrix}.  Examples of these objects are the predicted survival
#' events and probabilities returned by the \link{predict} function.
#' 
#' @seealso \code{\link{performance}}, \code{\link{metrics}}
#' 
SurvEvents <- function(object = numeric(), times = NULL) {
  object <- SurvMatrix(object, times)
  structure(object, class = c("SurvEvents", class(object)))
}


#' @rdname SurvMatrix
#' 
SurvProbs <- function(object = numeric(), times = NULL) {
  object <- SurvMatrix(object, times)
  structure(object, class = c("SurvProbs", class(object)))
}


mean.SurvProbs <- function(x, ...) {
  apply(x, 1, function(surv) surv_mean(time(x), surv))
}


predict.SurvProbs <- function(object, times, ...) {
  idx <- findInterval(times, time(object))
  cbind(1, object)[, idx + 1, drop = FALSE]
}


time.SurvMatrix <- function(x, ...) attr(x, "times")


#################### Survival Utility Functions ####################


cumsum_risk <- function(x) rev(cumsum(rev(x)))


surv_cases <- function(..., subset = TRUE) {
  df <- data.frame(...)
  is_finite <- Reduce("&", lapply(df, is.finite))
  df[subset & is_finite, , drop = FALSE]
}


surv_dist <- function(x = c("empirical", "exponential", "rayleigh",
                            "weibull")) {
  switch(match.arg(x),
         "empirical" = EmpiricalSurv,
         "exponential" = Exponential,
         "rayleigh" = Rayleigh,
         "weibull" = Weibull)
}


surv_dist_mean <- function(x = NULL) {
  surv_dist(if (is.null(x)) MachineShop::settings("dist.Surv") else x)
}


surv_dist_probs <- function(x = NULL) {
  surv_dist(if (is.null(x)) MachineShop::settings("dist.SurvProbs") else x)
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


surv_metric_mean <- function(x, times) {
  weights <- diff(c(0, times)) / tail(times, 1)
  sum(weights * x)
}


surv_times <- function(y) {
  sort(unique(y[y[, "status"] != 0, "time"]))
}
