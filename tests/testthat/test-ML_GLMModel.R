models <- c("GLMModel", "GLMStepAICModel")

test_that("model fitting", {
  skip_if_not(TEST_MODEL_FITTING)
  with_parallel({
    model <- "GLMModel"
    context(model)
    expect_output(test_model_binary(model))
    expect_output(test_model_factor(model))
    expect_output(test_model_numeric(model))
    expect_output(test_model_ordered(model))
    expect_error(test_model_Surv(model))

    model <- "GLMStepAICModel"
    context(model)
    expect_output(test_model_binary(model))
    expect_error(test_model_factor(model))
    expect_output(test_model_numeric(model))
    expect_error(test_model_ordered(model))
    expect_error(test_model_Surv(model))
  })
})
