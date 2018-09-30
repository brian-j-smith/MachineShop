TEST_MODEL_FITTING <- Sys.getenv("TEST_MODEL_FITTING") == "true"


data(Boston, package = "MASS", envir = environment())
data(Pima.tr, package = "MASS", envir = environment())
data(lung, package = "survival", envir = environment())


test_model <- function(formula, data, model, times = numeric()) {

  modelfit <- fit(formula, data, model)
  print(modelfit)
  
  if(!is(modelfit, "SVMModelFit")) {
    vi <- varimp(modelfit)
    print(vi)
    print(plot(vi))
  }
  
  pred <- head(predict(modelfit, data))
  print(pred)
  pred_prob <- head(predict(modelfit, data, type = "prob"))
  print(pred_prob)

  pred_times <- head(predict(modelfit, data, times = times))
  print(pred_times)
  pred_times_prob <- head(predict(modelfit, data, times = times, type = "prob"))
  print(pred_times_prob) 
  
  perf_boot <- resample(formula, data, model, BootControl)
  print(perf_boot)
  perf_cv <- resample(formula, data, model, CVControl)
  print(perf_cv)
  perf_oob <- resample(formula, data, model, OOBControl)
  print(perf_oob)
  
  print(summary(perf_cv))
  print(plot(perf_cv))
  
  perf_boot_times <- resample(formula, data, model,
                              BootControl(surv_times = times))
  print(perf_boot_times)
  perf_cv_times <- resample(formula, data, model,
                            CVControl(surv_times = times))
  print(perf_cv_times)
  perf_oob_times <- resample(formula, data, model,
                             OOBControl(surv_times = times))
  print(perf_oob_times)
  
  print(summary(perf_cv_times))
  print(plot(perf_cv_times))
  
}


test_model_factor <- function(model) {
  df <- iris
  df$Species <- factor(df$Species)
  fo <- Species ~ .
  test_model(fo, df, model)
}


test_model_factor2 <- function(model) {
  df <- Pima.tr
  df$type <- factor(df$type)
  fo <- type ~ .
  test_model(fo, df, model)
}


test_model_numeric <- function(model) {
  df <- Boston
  fo <- medv ~ .
  test_model(fo, df, model)
}


test_model_ordered <- function(model) {
  df <- Boston
  df$medv <- cut(df$medv, breaks = c(0, 15, 20, 25, 50), ordered = TRUE)
  fo <- medv ~ .
  test_model(fo, df, model)
}


test_model_Surv <- function(model) {
  df <- na.omit(lung)
  fo <- survival::Surv(time, status) ~ age + sex + ph.ecog + ph.karno +
    pat.karno + meal.cal + wt.loss
  test_model(fo, df, model, times = c(180, 360, 540))
}
