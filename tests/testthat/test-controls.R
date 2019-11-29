context("Resampling Controls")


library(MASS)
library(recipes)

df1 <- ICHomes
fo1 <- sale_amount ~ .
rec1 <- recipe(fo1, data = df1)

df2 <- iris
fo2 <- Species ~ .
rec2 <- recipe(fo2, data = df2)

df3 <- within(Melanoma, status <- status != 2)
fo3 <- Surv(time, status) ~ .
rec3 <- recipe(time + status ~ ., data = df3) %>%
  add_role(time, new_role = "surv_time") %>%
  add_role(status, new_role = "surv_event")

model <- GBMModel

controls <- c("BootControl",
              "BootOptimismControl",
              "CVControl",
              "CVOptimismControl",
              "OOBControl",
              "SplitControl",
              "TrainControl")


for (control in controls) {

  test_that("formula resampling", {
    skip_if_not(TEST_ALL)
    context(paste0(control, ": Formula"))
    with_parallel({
      expect_s4_class(resample(fo1, df1, model, control = control), "Resamples")
      expect_s4_class(resample(fo2, df2, model, control = control), "Resamples")
      expect_s4_class(resample(fo3, df3, model, control = control), "Resamples")
    })
  })

  test_that("recipe resampling", {
    skip_if_not(TEST_ALL)
    context(paste0(control, ": Recipe"))
    with_parallel({
      expect_s4_class(resample(rec1, model, control = control), "Resamples")
      expect_s4_class(resample(rec2, model, control = control), "Resamples")
      expect_s4_class(resample(rec3, model, control = control), "Resamples")
    })
  })

}
