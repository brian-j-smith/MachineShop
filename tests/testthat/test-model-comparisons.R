context("ResamplesHTest")


test_Resamples <- function() {
  fo <- factor(Species) ~ .
  control <- CVControl()
  
  gbmperf1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
  gbmperf2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
  gbmperf3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
  
  perf <- Resamples(GBM1 = gbmperf1, GBM2 = gbmperf2, GBM3 = gbmperf3)
  summary(perf)
  plot(perf)
  
  perfdiff <- diff(perf)
  summary(perfdiff)
  plot(perfdiff)
  t.test(perfdiff)
}


test_tune <- function() {
  library(survival)
  fo <- Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno +
    meal.cal + wt.loss
  gbmtune <- tune(fo,
                  data = lung,
                  model = GBMModel,
                  grid = expand.grid(n.trees = c(25, 50, 100),
                                     interaction.depth = 1:3,
                                     n.minobsinnode = c(5, 10)),
                  control = CVControl(folds = 10, repeats = 5,
                                      surv_times = c(180, 360, 540)))
  summary(gbmtune)
  plot(gbmtune, type = "line", metrics = c("ROC", "Brier"))
  
  gbmtunediff <- diff(gbmtune)
  summary(gbmtunediff)
  t.test(gbmtunediff)
}


test_that("model comparisons", {
  skip_if_not(TEST_MODEL_COMPARISONS)
  with_parallel({
    expect_is(test_Resamples(), "ResamplesHTest")
    expect_is(test_tune(), "ResamplesHTest")
  })
})
