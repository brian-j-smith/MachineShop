context("Model Comparisons")


test_Resamples <- function() {
  fo <- factor(Species) ~ .
  control <- CVControl()

  gbmperf1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
  gbmperf2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
  gbmperf3 <- resample(fo, iris, GBMModel(n.trees = 100), control)

  perf <- c(GBM1 = gbmperf1, GBM2 = gbmperf2, GBM3 = gbmperf3)
  summary(perf)
  plot(perf)

  perfdiff <- diff(perf)
  summary(perfdiff)
  plot(perfdiff)
  t.test(perfdiff)
}


test_TunedModel <- function() {
  library(MASS)
  fo <- medv ~ .

  gbmfit <- fit(fo, data = Boston,
                model = TunedModel(
                  GBMModel,
                  grid = expand.grid(n.trees = c(25, 50, 100),
                                     interaction.depth = 1:3,
                                     n.minobsinnode = c(5, 10)),
                  control = CVControl(folds = 10, repeats = 5)
                ))
  gbmtune <- as.MLModel(gbmfit)
  summary(gbmtune)
  plot(gbmtune, type = "line")

  gbmtunediff <- diff(gbmtune)
  summary(gbmtunediff)
  t.test(gbmtunediff[[1]])
}


test_that("Resamples differences", {
  skip_if_not(TEST_MODEL_COMPARISONS)
  with_parallel({
    expect_s4_class(test_Resamples(), "PerformanceDiffTest")
  })
})


test_that("TunedModel differences", {
  skip_if_not(TEST_MODEL_COMPARISONS)
  with_parallel({
    expect_s4_class(test_TunedModel(), "PerformanceDiffTest")
  })
})
