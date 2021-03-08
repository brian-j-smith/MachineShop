context("Tuning Grids")


library(dials)

param_list <- list(
  num.trees = trees(),
  mtry = mtry(),
  replace = new_qual_param("logical", c(TRUE, FALSE),
                           label = c(replace = "replace"))
)

test_fit <- function(grid) {
  model_fit <- expect_is(
    fit(sale_amount ~ ., data = ICHomes,
        model = TunedModel(RangerModel, grid = grid)),
    "MLModelFit"
  )
  model_grid <- as.MLModel(model_fit)@train_steps[[1]]@grid$Model
  nrow(model_grid) && ncol(model_grid) == nrow(grid)
}


test_that("ParameterGrid construction and model fitting", {
  skip_if_not(TEST_ALL)
  context("ParameterGrid")
  with_parallel({

    expect_error(ParameterGrid(param_list, size = -1))
    expect_error(ParameterGrid(param_list, random = 0))

    grid <- expect_s4_class(ParameterGrid(param_list), "ParameterGrid")
    expect_true(test_fit(grid))

    grid <- expect_s4_class(
      ParameterGrid(param_list, size = 0),
      "ParameterGrid"
    )
    expect_true(test_fit(grid))

    grid <- expect_s4_class(
      ParameterGrid(param_list, size = 2),
      "ParameterGrid"
    )
    expect_true(test_fit(grid))

    grid <- expect_s4_class(
      ParameterGrid(param_list, size = c(3, 0, 2)),
      "ParameterGrid"
    )
    expect_true(test_fit(grid))

    grid <- expect_s4_class(
      ParameterGrid(param_list, size = 2, random = 5),
      "ParameterGrid"
    )
    expect_true(test_fit(grid))

  })
})
