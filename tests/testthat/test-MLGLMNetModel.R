context("GLMNetModel")

model <- GLMNetModel(lambda = 0.01)

test_that("model fitting", {
  skip_if_not(TEST_MODEL_FITTING)
  with_parallel({
    test_model_factor(model)
    test_model_factor2(model)
    test_model_numeric(model)
    test_model_ordered(model)
    test_model_Surv(model)
  })
})
