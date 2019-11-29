context("Discrete Variables")


library(MASS)
library(recipes)

test_fit <- function(..., model, type) {
  get_fit_type <- switch(model@name,
                         "BlackBoostModel" = function(x) x$family@name,
                         "GAMBoostModel" = function(x) x$family@name,
                         "GBMModel" = function(x) x$distribution$name,
                         "GLMBoostModel" = function(x) x$family@name,
                         "GLMModel" = function(x) x$family$family,
                         "GLMNetModel" = function(x) class(x)[3],
                         "XGBModel" = function(x) x$params$objective
  )

  model_fit <- fit(..., model = model)
  obs <- response(model_fit)
  pred <- predict(model_fit)

  metrics <- c(mae, mse, msle, r2, rmse, rmsle)
  perf <- performance(obs, pred, metrics = metrics)

  if (is(obs, "BinomialMatrix")) {
    class <- "numeric"
    perf_mse <- mean((obs[, 1] / (obs[, 1] + obs[, 2]) - pred)^2)
  } else {
    class <- class(obs)
    perf_mse <- mean((obs - pred)^2)
  }
  startsWith(get_fit_type(model_fit), type) && class(pred) == class &&
    length(perf) == length(metrics) && perf["mse"] == perf_mse
}


test_that("BinomialMatrix construction and model fitting", {
  skip_if_not(TEST_ALL)
  context("BinomialMatrix")
  with_parallel({

    expect_s3_class(BinomialMatrix(), "BinomialMatrix")
    expect_s3_class(BinomialMatrix(1:10, 10), "BinomialMatrix")
    expect_s3_class(BinomialMatrix(1:10, 1:10), "BinomialMatrix")
    expect_error(BinomialMatrix(-1:10, 10))
    expect_error(BinomialMatrix(1:10, Inf))
    expect_error(BinomialMatrix(1:10, 1:5))
    expect_error(BinomialMatrix(1:10, 10:1))

    df <- esoph
    fo <- BinomialMatrix(ncases, size = ncases + ncontrols) ~ .
    expect_true(test_fit(fo, df, model = BlackBoostModel(), type = "Binomial"))
    expect_true(test_fit(fo, df, model = GAMBoostModel(baselearner = "bols"),
                         type = "Binomial"))
    expect_true(test_fit(fo, df, model = GLMBoostModel(), type = "Binomial"))
    expect_true(test_fit(fo, df, model = GLMModel(), type = "binomial"))
    expect_true(test_fit(fo, df, model = GLMNetModel(), type = "lognet"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resamples")

    rec <- recipe(ncases + n ~ ., data = within(df, n <- ncases + ncontrols)) %>%
      role_binom(count = ncases, size = n) %>%
      role_case(stratum = n) %>%
      step_rm(ncontrols)
    expect_true(test_fit(rec, model = BlackBoostModel(), type = "Binomial"))
    expect_true(test_fit(rec, model = GAMBoostModel(baselearner = "bols"),
                         type = "Binomial"))
    expect_true(test_fit(rec, model = GLMBoostModel(), type = "Binomial"))
    expect_true(test_fit(rec, model = GLMModel(), type = "binomial"))
    expect_true(test_fit(rec, model = GLMNetModel(), type = "lognet"))
    expect_s4_class(resample(rec, model = GLMModel), "Resamples")

  })
})


test_that("DiscreteVector construction and model fitting", {
  skip_if_not(TEST_ALL)
  context("DiscreteVector")
  with_parallel({

    expect_s4_class(DiscreteVector(), "DiscreteVector")
    expect_s4_class(DiscreteVector(1:10), "DiscreteVector")
    expect_s4_class(DiscreteVector(1:10, 1, 10), "DiscreteVector")
    expect_error(DiscreteVector(1:10, 5, 10))
    expect_error(DiscreteVector(1:10, 1, 5))

    df <- ICHomes
    fo <- DiscreteVector(sale_amount, min = 0) ~ .
    expect_true(test_fit(fo, df, model = GLMModel(), type = "gaussian"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resamples")

    rec <- recipe(
      sale_amount ~ .,
      data = within(df, sale_amount <- DiscreteVector(sale_amount, min = 0))
    )
    expect_true(test_fit(rec, model = GLMModel(), type = "gaussian"))
    expect_s4_class(resample(rec, model = GLMModel), "Resamples")

  })
})


test_that("NegBinomialVector construction and model fitting", {
  skip_if_not(TEST_ALL)
  context("NegBinomialVector")
  with_parallel({

    expect_s4_class(NegBinomialVector(), "NegBinomialVector")
    expect_s4_class(NegBinomialVector(0:10), "NegBinomialVector")

    df <- quine
    fo <- NegBinomialVector(Days) ~ .
    expect_true(test_fit(fo, df, model = BlackBoostModel(), type = "Negative"))
    expect_true(test_fit(fo, df, model = GAMBoostModel(baselearner = "bols"),
                         type = "Negative"))
    expect_true(test_fit(fo, df, model = GLMBoostModel(), type = "Negative"))
    expect_true(test_fit(fo, df, model = GLMModel(),
                         type = "Negative Binomial"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resamples")

    rec <- recipe(
      Days ~ .,
      data = within(df, Days <- NegBinomialVector(Days))
    )
    expect_true(test_fit(rec, model = BlackBoostModel(), type = "Negative"))
    expect_true(test_fit(rec, model = GAMBoostModel(baselearner = "bols"),
                         type = "Negative"))
    expect_true(test_fit(rec, model = GLMBoostModel(), type = "Negative"))
    expect_true(test_fit(rec, model = GLMModel(), type = "Negative Binomial"))
    expect_s4_class(resample(rec, model = GLMModel), "Resamples")

  })
})


test_that("PoissonVector construction and model fitting", {
  skip_if_not(TEST_ALL)
  context("PoissonVector")
  with_parallel({

    expect_s4_class(PoissonVector(), "PoissonVector")
    expect_s4_class(PoissonVector(0:10), "PoissonVector")

    df <- quine
    fo <- PoissonVector(Days) ~ .
    expect_true(test_fit(fo, df, model = BlackBoostModel(), type = "Poisson"))
    expect_true(test_fit(fo, df, model = GAMBoostModel(baselearner = "bols"),
                         type = "Poisson"))
    expect_true(test_fit(fo, df, model = GLMBoostModel(), type = "Poisson"))
    expect_true(test_fit(fo, df, model = GLMModel(), type = "poisson"))
    expect_true(test_fit(fo, df, model = GBMModel(), type = "poisson"))
    expect_true(test_fit(fo, df, model = GLMNetModel(), type = "fishnet"))
    expect_true(test_fit(fo, df, model = XGBModel(), type = "count:poisson"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resamples")

    rec <- recipe(
      Days ~ .,
      data = within(df, Days <- PoissonVector(Days))
    )
    expect_true(test_fit(rec, model = BlackBoostModel(), type = "Poisson"))
    expect_true(test_fit(rec, model = GAMBoostModel(baselearner = "bols"),
                         type = "Poisson"))
    expect_true(test_fit(rec, model = GLMBoostModel(), type = "Poisson"))
    expect_true(test_fit(rec, model = GLMModel(), type = "poisson"))
    expect_true(test_fit(rec, model = GBMModel(), type = "poisson"))
    expect_true(test_fit(rec, model = GLMNetModel(), type = "fishnet"))
    expect_true(test_fit(rec, model = XGBModel(), type = "count:poisson"))
    expect_s4_class(resample(rec, model = GLMModel), "Resamples")

  })
})
