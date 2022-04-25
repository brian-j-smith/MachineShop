## Discrete Variables

test_that("discrete variate construction and model fitting", {
  skip_if_not(TEST_ALL)
  with_parallel({

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

      if (is(obs, "BinomialVariate")) {
        class <- "numeric"
        perf_mse <- mean((obs[, 1] / (obs[, 1] + obs[, 2]) - pred)^2)
      } else {
        class <- class(obs)
        perf_mse <- mean((obs - pred)^2)
      }
      startsWith(get_fit_type(model_fit), type) && class(pred) == class &&
        length(perf) == length(metrics) &&
        all.equal(perf["mse"], perf_mse, check.attributes = FALSE)
    }


    ## BinomialVariate

    expect_s3_class(BinomialVariate(), "BinomialVariate")
    expect_s3_class(BinomialVariate(1:10, 10), "BinomialVariate")
    expect_s3_class(BinomialVariate(1:10, 1:10), "BinomialVariate")
    expect_error(BinomialVariate(-1:10, 10))
    expect_error(BinomialVariate(1:10, Inf))
    expect_error(BinomialVariate(1:10, 1:5))
    expect_error(BinomialVariate(1:10, 10:1))

    df <- esoph
    fo <- BinomialVariate(ncases, size = ncases + ncontrols) ~ .
    expect_true(test_fit(fo, df, model = BlackBoostModel(), type = "Binomial"))
    expect_true(test_fit(fo, df, model = GAMBoostModel(baselearner = "bols"),
                         type = "Binomial"))
    expect_true(test_fit(fo, df, model = GLMBoostModel(), type = "Binomial"))
    expect_true(test_fit(fo, df, model = GLMModel(), type = "binomial"))
    expect_true(test_fit(fo, df, model = GLMNetModel(), type = "lognet"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resample")

    rec <- recipe(ncases + n ~ ., data = within(df, n <- ncases + ncontrols)) %>%
      role_binom(x = ncases, size = n) %>%
      role_case(stratum = n) %>%
      step_rm(ncontrols)
    expect_true(test_fit(rec, model = BlackBoostModel(), type = "Binomial"))
    expect_true(test_fit(rec, model = GAMBoostModel(baselearner = "bols"),
                         type = "Binomial"))
    expect_true(test_fit(rec, model = GLMBoostModel(), type = "Binomial"))
    expect_true(test_fit(rec, model = GLMModel(), type = "binomial"))
    expect_true(test_fit(rec, model = GLMNetModel(), type = "lognet"))
    expect_s4_class(resample(rec, model = GLMModel), "Resample")


    ## DiscreteVariate

    expect_s4_class(DiscreteVariate(), "DiscreteVariate")
    expect_s4_class(DiscreteVariate(1:10), "DiscreteVariate")
    expect_s4_class(DiscreteVariate(1:10, 1, 10), "DiscreteVariate")
    expect_error(DiscreteVariate(1:10, 5, 10))
    expect_error(DiscreteVariate(1:10, 1, 5))

    df <- ICHomes
    fo <- DiscreteVariate(sale_amount, min = 0) ~ .
    expect_true(test_fit(fo, df, model = GLMModel(), type = "gaussian"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resample")

    rec <- recipe(
      sale_amount ~ .,
      data = within(df, sale_amount <- DiscreteVariate(sale_amount, min = 0))
    )
    expect_true(test_fit(rec, model = GLMModel(), type = "gaussian"))
    expect_s4_class(resample(rec, model = GLMModel), "Resample")


    ## NegBinomialVariate

    expect_s4_class(NegBinomialVariate(), "NegBinomialVariate")
    expect_s4_class(NegBinomialVariate(0:10), "NegBinomialVariate")

    df <- quine
    fo <- NegBinomialVariate(Days) ~ .
    expect_true(test_fit(fo, df, model = BlackBoostModel(), type = "Negative"))
    expect_true(test_fit(fo, df, model = GAMBoostModel(baselearner = "bols"),
                         type = "Negative"))
    expect_true(test_fit(fo, df, model = GLMBoostModel(), type = "Negative"))
    expect_true(test_fit(fo, df, model = GLMModel(),
                         type = "Negative Binomial"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resample")

    rec <- recipe(
      Days ~ .,
      data = within(df, Days <- NegBinomialVariate(Days))
    )
    expect_true(test_fit(rec, model = BlackBoostModel(), type = "Negative"))
    expect_true(test_fit(rec, model = GAMBoostModel(baselearner = "bols"),
                         type = "Negative"))
    expect_true(test_fit(rec, model = GLMBoostModel(), type = "Negative"))
    expect_true(test_fit(rec, model = GLMModel(), type = "Negative Binomial"))
    expect_s4_class(resample(rec, model = GLMModel), "Resample")


    ## PoissonVariate

    expect_s4_class(PoissonVariate(), "PoissonVariate")
    expect_s4_class(PoissonVariate(0:10), "PoissonVariate")

    df <- quine
    fo <- PoissonVariate(Days) ~ .
    expect_true(test_fit(fo, df, model = BlackBoostModel(), type = "Poisson"))
    expect_true(test_fit(fo, df, model = GAMBoostModel(baselearner = "bols"),
                         type = "Poisson"))
    expect_true(test_fit(fo, df, model = GLMBoostModel(), type = "Poisson"))
    expect_true(test_fit(fo, df, model = GLMModel(), type = "poisson"))
    expect_true(test_fit(fo, df, model = GBMModel(), type = "poisson"))
    expect_true(test_fit(fo, df, model = GLMNetModel(), type = "fishnet"))
    expect_true(test_fit(fo, df, model = XGBModel(), type = "count:poisson"))
    expect_s4_class(resample(fo, df, model = GLMModel), "Resample")

    rec <- recipe(
      Days ~ .,
      data = within(df, Days <- PoissonVariate(Days))
    )
    expect_true(test_fit(rec, model = BlackBoostModel(), type = "Poisson"))
    expect_true(test_fit(rec, model = GAMBoostModel(baselearner = "bols"),
                         type = "Poisson"))
    expect_true(test_fit(rec, model = GLMBoostModel(), type = "Poisson"))
    expect_true(test_fit(rec, model = GLMModel(), type = "poisson"))
    expect_true(test_fit(rec, model = GBMModel(), type = "poisson"))
    expect_true(test_fit(rec, model = GLMNetModel(), type = "fishnet"))
    expect_true(test_fit(rec, model = XGBModel(), type = "count:poisson"))
    expect_s4_class(resample(rec, model = GLMModel), "Resample")

  })
})
