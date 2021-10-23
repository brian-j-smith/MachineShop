test_that("model fitting", {
  skip_if_not(TEST_MODEL_FITTING)
  with_parallel({
    model <- BARTModel(K = 10, keepevery = 1)
    expect_output(test_model_binary(model))
    expect_output(test_model_factor(model))
    expect_output(test_model_numeric(model))
    expect_output(test_model_Surv(model))
  })
})
