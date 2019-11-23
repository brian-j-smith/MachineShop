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
  grid_lengths <- rep(grid@length, length.out = nrow(grid))
  model_grid <- as.MLModel(model_fit)@trainbits@grid
  nrow(model_grid) && ncol(model_grid) == nrow(grid)
}


test_that("ParamSet construction and model fitting", {
  skip_if_not(TEST_ALL)
  context("ParamSet")
  with_parallel({
    
    expect_error(ParamSet(param_list, length = -1))
    expect_error(ParamSet(param_list, random = 0))
    
    grid <- expect_s4_class(ParamSet(param_list), "ParamSet")
    expect_true(test_fit(grid))
    
    grid <- expect_s4_class(
      ParamSet(param_list, length = 0),
      "ParamSet"
    )
    expect_true(test_fit(grid))
    
    grid <- expect_s4_class(
      ParamSet(param_list, length = 2),
      "ParamSet"
    )
    expect_true(test_fit(grid))
    
    grid <- expect_s4_class(
      ParamSet(param_list, length = c(3, 0, 2)),
      "ParamSet"
    )
    expect_true(test_fit(grid))
    
    grid <- expect_s4_class(
      ParamSet(param_list, length = 2, random = 5),
      "ParamSet"
    )
    expect_true(test_fit(grid))
    
  })
})
