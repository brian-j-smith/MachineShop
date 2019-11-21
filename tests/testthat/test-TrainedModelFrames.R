context("Trained Model Frames")


df <- ICHomes
fo1 <- sale_amount ~ sale_year + built + style + construction
fo2 <- sale_amount ~ sale_year + base_size + bedrooms + basement

model <- SelectedModel(
  GLMModel,
  TunedModel(GBMModel),
  SelectedModel(
    NNetModel,
    SVMModel
  )
)


test_that("SelectedFormula training", {
  skip_if_not(TEST_TRAINING)
  context("SelectedFormula")
  with_parallel({
    inputs <- SelectedFormula(fo1, fo2, data = df)
    expect_is(fit(inputs, model = model), "MLModelFit")
  })
})


test_that("SelectedMatrix training", {
  skip_if_not(TEST_TRAINING)
  context("SelectedMatrix")
  with_parallel({
    inputs <- SelectedMatrix(
      model.matrix(~ sale_year + built + style + construction, ICHomes)[, -1],
      model.matrix(~ sale_year + base_size + bedrooms + basement, ICHomes)[, -1],
      y = ICHomes$sale_amount
    )
    expect_is(fit(inputs, model = model), "MLModelFit")
  })
})


test_that("SelectedModelFrame training", {
  skip_if_not(TEST_TRAINING)
  context("SelectedModelFrame")
  with_parallel({
    inputs <- SelectedModelFrame(
      ModelFrame(fo1, data = df),
      ModelFrame(fo2, data = df)
    )
    expect_is(fit(inputs, model = model), "MLModelFit")
  })
})
