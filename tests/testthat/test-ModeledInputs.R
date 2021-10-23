## Modeled Inputs


test_that("ModeledInput fitting", {
  skip_if_not(TEST_TRAINING)
  with_parallel({

    library(recipes)

    df <- ICHomes
    fo <- sale_amount ~ .
    x <- model.matrix(fo, df)[, -1]
    y <- model.response(model.frame(fo, df))
    mf <- ModelFrame(fo, data = df)
    rec <- recipe(fo, data = df)

    model <- "GLMModel"

    input <- expect_is(ModeledInput(fo, df, model = model), "ModeledFrame")
    expect_is(fit(input), "MLModelFit")
    input <- expect_is(ModeledInput(x, y, model = model), "ModeledFrame")
    expect_is(fit(input), "MLModelFit")
    input <- expect_is(ModeledInput(mf, model = model), "ModeledFrame")
    expect_is(fit(input), "MLModelFit")
    input <- expect_is(ModeledInput(rec, model = model), "ModeledRecipe")
    expect_is(fit(input), "MLModelFit")

  })
})
