context("Recursive Feature Elimination")


library(recipes)
library(survival)

model <- GBMModel

expect_output <- function(x, nrow = NULL) {
  pass <- is.data.frame(x) &&
    nrow(x) > 0 &&
    is.numeric(x$size) &&
    all(diff(x$size) < 0) &&
    is.list(x$terms) &&
    all(mapply(is.character, x$terms)) &&
    all(lengths(x$terms) == x$size) &&
    is.logical(x$optimal) &&
    sum(x$optimal) == 1 &&
    is.numeric(x$performance)
  if (!is.null(nrow)) pass <- pass && nrow(x) == nrow
  expect_true(pass)
  x
}


test_that("test rfe", {
  skip_if_not(TEST_ALL)
  with_parallel({

    context("test matrix")
    x <- model.matrix(sale_amount ~ . - 1, data = ICHomes)
    y <- ICHomes$sale_amount
    rfe(x, y, model = model) %>% expect_output()

    context("test recipe")
    rfe(recipe(sale_amount ~ ., data = ICHomes), model = model) %>%
      expect_output()

    context("test SelectedModelFrame")
    sel_mf <- SelectedInput(
      sale_amount ~ sale_year + built + style + construction,
      sale_amount ~ sale_year + base_size + bedrooms + basement,
      data = ICHomes
    )
    rfe(sel_mf, model = model) %>% expect_output()

    context("test SelectedModelRecipe")
    sel_rec <- SelectedInput(
      recipe(sale_amount ~ sale_year + built + style + construction,
             data = ICHomes),
      recipe(sale_amount ~ sale_year + base_size + bedrooms + basement,
             data = ICHomes)
    )
    rfe(sel_rec, model = model) %>% expect_output()

    context("test survival")
    rfe(Surv(time, status) ~ ., data = veteran, model = model) %>%
      expect_output()
    rfe(Surv(time, status) ~ ., data = veteran, model = model,
        control = CVControl() %>%
          set_predict(times = quantile(veteran$time, 1:2 / 3))) %>%
      expect_output()
    rfe(Surv(time, status) ~ ., data = veteran, model = model,
        control = CVControl() %>% set_predict(times = median(veteran$time))) %>%
      expect_output()

    context("test defaults")
    rfe_default <- function(...) {
      rfe(sale_amount ~ ., data = ICHomes, model = model, ...)
    }
    res <- rfe_default() %>% expect_output()
    n <- nrow(res)

    context("test props")
    rfe_default(props = c(1, 1)) %>% expect_output(nrow = 1)
    rfe_default(props = 1:2) %>% expect_error()
    rfe_default(props = 1:3 / 3) %>% expect_output(nrow = 3)
    rfe_default(props = 0:3 / 3) %>% expect_error()

    context("test sizes")
    rfe_default(sizes = 0) %>% expect_error()
    rfe_default(sizes = c(8, 8, 12, 100)) %>% expect_output(nrow = 3)

    context("test recompute")
    rfe_default(recompute = TRUE) %>% expect_output(nrow = n)

    context("test optimize")
    rfe_default(optimize = "local") %>% expect_output()

    context("test samples")
    rfe_default(samples = 2) %>% expect_output(nrow = n)
    rfe_default(samples = c(2, 2)) %>% expect_output(nrow = n)
    rfe_default(samples = c(rfe = 0)) %>% expect_error()
    rfe_default(samples = c(varimp = 0)) %>% expect_error()

    context("test metrics")
    rfe_default(metrics = mse) %>% expect_output(nrow = n)

    context("test stat")
    rfe_default(
      samples = c(2, 2),
      stat = function(x) mean(x) + sd(x) / sqrt(length(x))
    ) %>% expect_output(nrow = n)
    rfe_default(stat = identity) %>% expect_error()

  })

})
