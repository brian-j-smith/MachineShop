## Offset Terms

test_that("test model offsets", {
  skip_if_not(TEST_ALL)

  library(dials)
  library(recipes)

  df <- esoph
  fo <- PoissonVariate(ncases) ~ agegp + alcgp + tobgp +
    offset(log(ncases + ncontrols))

  rec_df <- within(esoph, {
    offset <- log(ncases + ncontrols)
    ncases <- PoissonVariate(ncases)
  })
  rec <- recipe(ncases ~ agegp + alcgp + tobgp + offset, data = rec_df) %>%
    role_pred(offset = offset, replace = TRUE)

  test_fit <- function(model) {
    has_offset <- switch(model@name,
                         "GBMModel" = {
                           function(x) !is.null(attr(x$Terms, "offset"))
                         },
                         "GLMModel" = function(x) !is.null(x$offset),
                         "GLMNetModel" = function(x) !is.null(x$offset))


    model_fit <- expect_is(fit(fo, df, model = model), "MLModelFit")
    expect_true(has_offset(model_fit))
    expect_s4_class(predict(model_fit), "PoissonVariate")

    model_fit <- expect_is(fit(rec, model = model), "MLModelFit")
    expect_true(has_offset(model_fit))
    expect_s4_class(predict(model_fit), "PoissonVariate")
  }

  ## GBMModel fitting and prediction
  test_fit(GBMModel())

  ## GLMModel fitting and prediction
  test_fit(GLMModel())

  ## GLMMetModel fitting and prediction
  test_fit(GLMNetModel())

})
