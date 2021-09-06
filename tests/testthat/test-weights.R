context("Case Weighted Metrics")


library(survival)


n <- 100
weights <- 1:n


test_metrics <- function(obs, pred, weights) {
  skip_if_not(TEST_ALL)
  metrics <- names(metricinfo(obs, pred))
  perf1 <- performance(obs, pred, metrics = metrics)
  perf2 <- performance(obs, pred, weights, metrics = metrics)
  pass <- perf1 != perf2
  list(pass = all(pass), values = cbind(perf1, perf2, pass))
}


test_that("performance comparisons with and without weights", {
  skip_if_not(TEST_ALL)

  context("Observed factor, Predicted factor")
  obs <- factor(rbinom(n, 2, 0.5))
  pred <- factor(rbinom(n, 2, 0.5))
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)

  context("Observed factor, Predicted matrix")
  obs <- factor(rbinom(n, 2, 0.5))
  pred <- matrix(runif(3 * n), ncol = 3)
  pred <- pred / rowSums(pred)
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)

  context("Observed factor, Predicted matrix")
  obs <- factor(rbinom(n, 1, 0.5))
  pred <- runif(n)
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)

  context("Observed matrix, Predicted matrix")
  obs <- matrix(rgamma(2 * n, 1, 1), ncol = 2)
  pred <- matrix(rgamma(2 * n, 1, 1), ncol = 2)
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)

  context("Observed numeric, Predicted numeric")
  obs <- rgamma(n, 1, 1)
  pred <- rgamma(n, 1, 1)
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)

  context("Observed Surv, Predicted numeric")
  obs <- Surv(rgamma(n, 1, 1), rbinom(n, 1, 0.5))
  pred <- rgamma(n, 1, 1)
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)

  context("Observed Surv, Predicted SurvEvents")
  obs <- Surv(rgamma(n, 1, 1), rbinom(n, 1, 0.5))
  pred <- SurvEvents(matrix(runif(2 * n), ncol = 2) > 0.5, times = 1:2)
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)

  context("Observed Surv, Predicted SurvProbs")
  obs <- Surv(rgamma(n, 1, 1), rbinom(n, 1, 0.5))
  pred <- SurvProbs(matrix(runif(2 * n), ncol = 2), times = 1:2)
  res <- test_metrics(obs, pred, weights)
  expect_true(res$pass)
})
