TEST_ALL <- Sys.getenv("TEST_ALL") == "true"
TEST_MODEL_FITTING <- TEST_ALL || Sys.getenv("TEST_MODEL_FITTING") == "true"
TEST_MODEL_COMPARISONS <-
  TEST_ALL || Sys.getenv("TEST_MODEL_COMPARISONS") == "true"


data(Boston, package = "MASS", envir = environment())
data(Pima.tr, package = "MASS", envir = environment())
data(Melanoma, package = "MASS", envir = environment())


test_model <- function(formula, data, model, times = numeric()) {

  modelfit <- fit(formula, data, model)
  print(modelfit)
  
  vi <- varimp(modelfit)
  print(vi)
  print(plot(vi))

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
  test_model(factor(Species) ~ ., data = iris, model = model)
}


test_model_factor2 <- function(model) {
  test_model(factor(type) ~ ., data = Pima.tr, model = model)
}


test_model_numeric <- function(model) {
  test_model(medv ~ ., data = Boston, model = model)
}


test_model_ordered <- function(model) {
  df <- Boston
  df$medv <- cut(Boston$medv, breaks = c(0, 15, 20, 25, 50), ordered = TRUE)
  test_model(medv ~ ., data = df, model = model)
}


test_model_Surv <- function(model) {
  test_model(survival::Surv(time, status != 2) ~ sex + age + year + thickness + ulcer,
             data = Melanoma, model = model, times = 365 * c(2, 5, 10))
}
