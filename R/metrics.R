rocSurv <- function(observed, predicted, time) {
  survivalROC(observed[,"time"], observed[,"status"], 1 - predicted,
              predict.time = time, method = "KM")$AUC
}


brierSurv <- function(observed, predicted, time) {
  obs_times <- observed[,"time"]
  obs_events <- observed[,"status"]
  fitcens <- survfit(Surv(obs_times, 1 - obs_events) ~ 1)
  is_obs_after <- obs_times > time
  weights <- (obs_events == 1 | is_obs_after) /
    probs.survfit(fitcens, pmin(obs_times, time))
  mean(weights * (is_obs_after - predicted)^2)
}


meanSurvMetric <- function(x, times) {
  weights <- diff(c(0, times)) / tail(times, 1)
  sum(weights * x)
}
