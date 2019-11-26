context("Offset Terms")


library(dials)
library(recipes)

df <- esoph
fo <- PoissonVector(ncases) ~ agegp + alcgp + tobgp +
  offset(log(ncases + ncontrols))

rec_df <- within(esoph, {
  offset <- log(ncases + ncontrols)
  ncases <- PoissonVector(ncases)
})
rec <- recipe(ncases ~ agegp + alcgp + tobgp + offset, data = rec_df) %>%
  update_role(offset, new_role = "offset")

test_fit <- function(model) {
  has_offset <- switch(model@name,
                       "GBMModel" = {
                         function(x) !is.null(attr(x$Terms, "offset"))
                        },
                       "GLMModel" = function(x) !is.null(x$offset),
                       "GLMNetModel" = function(x) !is.null(x$offset))


  model_fit <- expect_is(fit(fo, df, model = model), "MLModelFit")
  expect_true(has_offset(model_fit))
  expect_s4_class(predict(model_fit), "PoissonVector")
  
  model_fit <- expect_is(fit(rec, model = model), "MLModelFit")
  expect_true(has_offset(model_fit))
  expect_s4_class(predict(model_fit), "PoissonVector")
}


test_that("GBMModel fitting and prediction", {
  skip_if_not(TEST_ALL)
  test_fit(GBMModel())
})


test_that("GLMModel fitting and prediction", {
  skip_if_not(TEST_ALL)
  test_fit(GLMModel())
})


test_that("GLMMetModel fitting and prediction", {
  skip_if_not(TEST_ALL)
  test_fit(GLMNetModel())
})
