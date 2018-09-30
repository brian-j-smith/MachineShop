models <- c("CoxModel", "CoxStepAICModel")

for(model in models) {
  context(model)
  
  test_that("model fitting", {
    skip_if_not(TEST_MODEL_FITTING)
    with_parallel({
      expect_error(test_model_factor(model))
      expect_error(test_model_factor2(model))
      expect_error(test_model_numeric(model))
      expect_error(test_model_ordered(model))
      test_model_Surv(model)
    })
  })
}
