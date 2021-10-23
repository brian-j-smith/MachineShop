test_that("model fitting", {
  skip_if_not(TEST_MODEL_FITTING)
  with_parallel({
    models <- c("CoxModel", "CoxStepAICModel")
    for (model in models) {
      expect_error(test_model_binary(model))
      expect_error(test_model_factor(model))
      expect_error(test_model_numeric(model))
      expect_error(test_model_ordered(model))
      expect_output(test_model_Surv(model))
    }
  })
})
