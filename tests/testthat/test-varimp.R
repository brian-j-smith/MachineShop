## Variable Importance

test_that("test permuation-based varimp", {
  skip_if_not(TEST_ALL)
  with_parallel({

    library(recipes)

    fo <- sale_amount ~ .
    data <- ICHomes
    model <- SVMRadialModel

    test_varimp <- function(x, ...) {
      set.seed(123)
      vi <- varimp(x, ..., method = "permute", scale = FALSE)
      expect_true(is(vi, "VariableImportance") && !any(is.na(vi)))
      vi
    }

    test_equal <- function(x, y) {
      expect_true(all(x == y))
    }

    test_block <- function(x) {
      test_varimp(x)
      test_varimp(x, metric = mse)
      test_varimp(x, compare = "/")

      vi1 <- test_varimp(x, samples = 10)
      vi2 <- test_varimp(x, samples = 10, stats = c(Mean = mean, SD = sd))
      test_equal(vi1, vi2[, 1])
      test_equal(
        vi1,
        test_varimp(x, samples = 10, na.rm = FALSE)
      )
      test_equal(
        vi2,
        test_varimp(x, samples = 10, stats = c(Mean = mean, SD = sd), na.rm = FALSE)
      )

      test_equal(
        test_varimp(x, samples = 10, prop = 0.25),
        test_varimp(x, samples = 10, size = nrow(ICHomes) * 0.25)
      )
    }

    fo_fit <- fit(fo, data = data, model = model)
    rec <- recipe(fo, data = data) %>%
      step_pca(all_numeric_predictors())
    rec_fit <- fit(rec, model = model)

    test_block(fo_fit)
    test_block(rec_fit)

  })
})
