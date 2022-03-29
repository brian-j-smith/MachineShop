## ModelSpecification class

test_that("test ModelSpecification", {
  skip_if_not(TEST_ALL)
  with_parallel({

    library(recipes)

    df <- ICHomes
    fo <- sale_amount ~ .

    test_modelspec <- function(...) {
      ModelSpecification(..., model = "GLMModel") %>%
        expect_is("ModelSpecification") %>%
        fit %>%
        expect_is("MLModelFit")
    }

    test_modelspec(fo, data = df)
    test_modelspec(
      x = model.matrix(fo, df)[, -1],
      y = model.response(model.frame(fo, df))
    )
    test_modelspec(ModelFrame(fo, data = df))
    test_modelspec(recipe(fo, data = df))

    modelspec <- ModelSpecification(
      input = TunedInput(
        recipe(fo, data = df) %>%
          step_pca(all_numeric_predictors(), id = "pca"),
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
