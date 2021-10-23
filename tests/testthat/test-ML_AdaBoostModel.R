test_that("model fitting", {
  skip_if_not(TEST_MODEL_FITTING)
  with_parallel({
    model <- AdaBoostModel(mfinal = 10)
    expect_output(test_model_factor(model))
    expect_error(test_model_numeric(model))
    expect_error(test_model_Surv(model))
  })
})
