## ModelSpecification class

test_that("test ModelSpecification", {
  skip_if_not(TEST_ALL)
  with_parallel({

    library(recipes)

    test_NullControl <- function(fun) {
      input <- ModelFrame(sale_amount ~ ., data = ICHomes)
      model <- GBMModel()
      modelspec <- ModelSpecification(input, model, control = NULL)
      set.seed(123)
      res1 <- fun(modelspec)
      set.seed(123)
      res2 <- fun(input, model)
      expect_identical(res1, res2)
    }

    test_NullControl(function(...) predict(fit(...)))
    test_NullControl(resample)
    test_NullControl(rfe)

    modelspec <- ModelSpecification(
      input = TunedInput(
        recipe(sale_amount ~ ., data = ICHomes) %>%
          step_pca(all_numeric(), -all_outcomes(), id = "pca"),
        grid = expand_steps(pca = list(num_comp = 6:7))
      ),
      model = TunedModel(
        GBMModel,
        grid = c(n.trees = 3, interaction.depth = 3)
      )
    )

    expect_true(is(modelspec@grid, "tbl_df"))
    expect_true(nrow(modelspec@grid) == 18)
    expect_true(length(modelspec@grid) == 2)
    expect_true(length(modelspec@grid[[2]]) == 2)

    test_optim <- function(set, ...) {
      modelspec <- set(modelspec, ...)
      mlmodel <- as.MLModel(fit(modelspec))
      step <- mlmodel@steps[[1]]
      expect_true(step@method == modelspec@params@optim@label &&
        is(step@log$params, "tbl_df") &&
        length(step@log$params) &&
        nrow(step@log$params))
    }

    test_optim(set_optim_bayes, package = "ParBayesianOptimization")
    test_optim(set_optim_bayes, package = "rBayesianOptimization")
    test_optim(set_optim_bfgs)
    test_optim(set_optim_grid)
    test_optim(set_optim_grid, random = 10)
    test_optim(set_optim_pso, each = 3)
    test_optim(set_optim_sann)

    test_set_control <- function(name, ...) {
      params <- list(...)
      modelspec <- do.call(paste0("set_", name), c(list(modelspec), params))
      control_params <- slot(modelspec@params@control, name)
      expect_equal(control_params[names(params)], params[names(params)])
    }

  })
})
