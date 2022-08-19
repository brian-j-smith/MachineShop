test_that("check ParsnipModel predictions", {

  skip_if_not(TEST_MODEL_FITTING)

  library(censored)
  library(MASS)
  library(survival)

  compare_models <- function(fo, data, model1, model2, ...) {
    fit1 <- MachineShop::fit(fo, data, model = model1)
    fit2 <- MachineShop::fit(fo, data, model = model2)
    for (type in c("response", "default")) {
      expect_equal(
        S3Part(predict(fit1, type = type, ...), strictS3 = TRUE),
        S3Part(predict(fit2, type = type, ...), strictS3 = TRUE)
      )
    }
  }

  ## numeric response
  compare_models(
    fo = sale_amount ~ .,
    data = ICHomes,
    model1 = ParsnipModel(parsnip::linear_reg()),
    model2 = GLMModel()
  )

  ## multivariate response
  compare_models(
    fo = cbind(mpg, hp, qsec) ~ .,
    data = mtcars,
    model1 = ParsnipModel(parsnip::linear_reg()),
    model2 = GLMModel()
  )

  ## dichotomous response
  compare_models(
    fo = type ~ .,
    data = Pima.tr,
    model1 = ParsnipModel(parsnip::logistic_reg()),
    model2 = GLMModel()
  )

  ## multinomial response
  compare_models(
    fo = Species ~ .,
    data = iris,
    model1 = ParsnipModel(parsnip::multinom_reg()),
    model2 = GLMModel()
  )

  ## survival response
  fo <- Surv(time, status) ~ .
  data <- veteran

  model <- ParsnipModel(parsnip::survival_reg(), dist = "weibull")

  model_fit <- MachineShop::fit(fo, data = data, model = model)
  pred <- predict(model_fit)
  expect_s4_class(pred, "SurvTimes")
  expect_vector(pred, size = nrow(veteran))

  compare_models(
    fo = fo,
    data = data,
    model1 = model,
    model2 = SurvRegModel(),
    times = c(100, 200)
  )

  model <- ParsnipModel(parsnip::proportional_hazards())

  compare_models(
    fo = fo,
    data = data,
    model1 = model,
    model2 = CoxModel(),
    times = c(100, 200)
  )

})
