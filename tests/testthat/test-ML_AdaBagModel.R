context("AdaBagModel")

model <- AdaBagModel(mfinal = 10)

test_that("model fitting", {
  skip_if_not(TEST_MODEL_FITTING)
  with_parallel({
    expect_output(test_model_factor(model))
    expect_error(test_model_numeric(model))
    expect_error(test_model_Surv(model))
  })
})
