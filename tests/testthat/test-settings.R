## Global Settings

test_that("settings changes and views", {
  skip_if_not(TEST_ALL)

  presets <- settings()

  expect_identical(settings("control", cut = 0.25, print_max = 5),
                   presets[c("control", "cutoff", "print_max")])
  expect_identical(settings("cutoff"), 0.25)
  expect_identical(settings("print_max"), 5)

  new_value <- "BootControl"
  expect_type(settings(control = new_value), "list")
  expect_identical(settings("control"), new_value)
  expect_error(settings(control = "MLModel"))

  new_value <- 0.05
  expect_type(settings(cutoff = new_value), "list")
  expect_identical(settings("cutoff"), new_value)
  expect_error(settings(cutoff = Inf))
  expect_error(settings(cutoff = character()))

  new_value <- "exponential"
  expect_type(settings(distr.SurvMeans = substr(new_value, 1, 3)), "list")
  expect_identical(settings("distr.SurvMeans"), new_value)
  expect_error(settings(distr.SurvMeans = character()))

  new_value <- "exponential"
  expect_type(settings(distr.SurvProbs = substr(new_value, 1, 3)), "list")
  expect_identical(settings("distr.SurvProbs"), new_value)
  expect_error(settings(distr.SurvProbs = character()))

  expect_type(settings(grid = 5), "list")
  expect_s4_class(settings("grid"), "Grid")
  expect_type(settings(grid = Grid), "list")
  expect_s4_class(settings("grid"), "Grid")
  expect_error(settings(grid = character()))

  new_value <- "breslow"
  expect_type(settings(method.EmpiricalSurv = substr(new_value, 1, 3)), "list")
  expect_identical(settings("method.EmpiricalSurv"), new_value)
  expect_error(settings(method.EmpiricalSurv = character()))

  new_value <- c(mse, "mse")
  expect_type(settings(metrics.ConfusionMatrix = new_value), "list")
  expect_identical(settings("metrics.ConfusionMatrix"), new_value)
  expect_error(settings(metrics.ConfusionMatrix = "mean"))

  new_value <- c(mse, "mse")
  expect_type(settings(metrics.factor = new_value), "list")
  expect_identical(settings("metrics.factor"), new_value)
  expect_error(settings(metrics.factor = "mean"))

  new_value <- c(mse, "mse")
  expect_type(settings(metrics.matrix = new_value), "list")
  expect_identical(settings("metrics.matrix"), new_value)
  expect_error(settings(metrics.matrix = "mean"))

  new_value <- c(mse, "mse")
  expect_type(settings(metrics.numeric = new_value), "list")
  expect_identical(settings("metrics.numeric"), new_value)
  expect_error(settings(metrics.numeric = "mean"))

  new_value <- c(mse, "mse")
  expect_type(settings(metrics.Surv = new_value), "list")
  expect_identical(settings("metrics.Surv"), new_value)
  expect_error(settings(metrics.Surv = "mean"))

  expect_type(settings(print_max = 5), "list")
  expect_identical(settings("print_max"), 5)
  expect_type(settings(print_max = Inf), "list")
  expect_identical(settings("print_max"), Inf)
  expect_error(settings(print_max = character()))

  old_values <- settings("require")
  expect_type(settings(require = c("MachineShop", "stats")), "list")
  expect_identical(settings("require"), c("stats", old_values))
  expect_error(settings(require = 1))

  settings("reset")
  values <- c("cut", "grid")
  old_values <- settings(values)
  settings(control = "BootControl", cutoff = 0.1, grid = 10)
  settings(reset = values)
  expect_identical(settings("control"), "BootControl")
  expect_identical(settings(values), old_values)

  new_value <- "median"
  expect_type(settings(stat.Curve = new_value), "list")
  expect_identical(settings("stat.Curve"), new_value)
  expect_error(settings(stat.Curve = "character"))

  new_value <- "median"
  expect_type(settings(stat.Resample = new_value), "list")
  expect_identical(settings("stat.Resamples"), new_value)
  expect_error(settings(stat.Resamples = "character"))

  new_value <- "median"
  expect_type(settings(stat.Trained = new_value), "list")
  expect_identical(settings("stat.Trained"), new_value)
  expect_error(settings(stat.Trained = "character"))

  new_value <- c("median", sd)
  expect_type(settings(stats.PartialDependence = new_value), "list")
  expect_identical(settings("stats.PartialDependence"), new_value)
  expect_error(settings(stats.PartialDependence = "character"))

  new_value <- c("median", sd)
  expect_type(settings(stats.Resamples = new_value), "list")
  expect_identical(settings("stats.Resamples"), new_value)
  expect_error(settings(stats.Resamples = "character"))

  settings("reset")
  expect_identical(settings(), presets)

})
