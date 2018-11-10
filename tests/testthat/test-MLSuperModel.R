context("SuperModel")

test_that("model fitting", {
  skip_if_not(TEST_MODEL_FITTING)
  with_parallel({
    model <- SuperModel(GBMModel, SVMRadialModel, GLMModel)
    expect_output(test_model_factor(model))
    expect_output(test_model_factor2(model))
    expect_output(test_model_numeric(model))
    
    model <- SuperModel(RandomForestModel, POLRModel)
    expect_output(test_model_ordered(model))
    
    model <- SuperModel(GBMModel, GLMNetModel(lambda = 0.01))
    expect_output(test_model_Surv(model))
  })
})
