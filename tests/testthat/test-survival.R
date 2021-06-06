context("Survival Analysis")


library(MASS)
library(recipes)
library(survival)

df <- within(Melanoma, status <- status != 2)
fo <- Surv(time, status) ~ sex + age + year + thickness + ulcer
rec <- recipe(time + status ~ sex + age + year + thickness + ulcer, data = df) %>%
  role_surv(time = time, event = status) %>%
  role_case(stratum = status)
times <- 365 * c(2, 5, 10)

output <- function(obs, pred) {
  print(pred)
  print(performance(obs, pred))
}

test_predict_all <- function(..., model) {

  cat("Model:", model@name, "\n")
  invisible(capture.output(model_fit <- fit(..., model = model)))
  obs <- response(model_fit)

  for (distr in c("emp", "exp", "ray", "wei")) {
    cat("\nPredicted means (distr = ", distr, ")\n", sep = "")
    output(obs, predict(model_fit, distr = distr))
    cat("\nPredicted probabilities (distr = ", distr, ")\n", sep = "")
    output(obs, predict(model_fit, times = times, type = "prob", distr = distr))
  }

  for (method in c("bre", "efr")) {
    cat("\nPredicted means (method = ", method, ")\n", sep = "")
    output(obs, predict(model_fit, method = method))
    cat("\nPredicted probabilities (method = ", method, ")\n", sep = "")
    output(obs, predict(model_fit, times = times, type = "prob", method = method))
  }

  cat("\nPredicted events:", "\n")
  output(obs, predict(model_fit, times = times))

}

test_predict_defaults <- function(..., model) {
  cat("Model:", model@name, "\n")
  invisible(capture.output(model_fit <- fit(..., model = model)))
  output(response(model_fit), predict(model_fit))
}


verify_output(test_path("test-survival.txt"), {
  skip_if_not(TEST_ALL)
  set.seed(123)
  cat("Recipe Specification", "\n")
  test_predict_all(rec, model = CoxModel())
  cat("Formula Specification", "\n")
  models <- c(CoxModel(), GBMModel(), CForestModel(), SurvRegModel(),
              XGBDARTModel(), XGBLinearModel(updater = "coord_descent"),
              XGBTreeModel())
  for (model in models) {
    test_predict_all(fo, df, model = model)
  }
  models <- c(BARTModel(), BlackBoostModel(),
              GAMBoostModel(baselearner = "bols"), GLMBoostModel(),
              GLMNetModel(lambda = 0.05), RangerModel(), RPartModel())
  for (model in models) {
    test_predict_defaults(fo, df, model = model)
  }
})
