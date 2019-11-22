context("Trained Recipes")


library(recipes)

rec1 <- recipe(sale_amount ~ ., data = ICHomes)
rec2 <- rec1 %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_pca(all_numeric(), -all_outcomes(), id = "pca")

sel_rec <- SelectedRecipe(rec1, rec2)
tun_rec <- TunedRecipe(rec2, grid = expand_steps(pca = list(num_comp = 1:4)))


models <- c(
  "GLMModel" = GLMModel,
  "TunedModel(GBM)" = TunedModel(GBMModel),
  "SelectedModel(SVMModel, RangerModel)" = SelectedModel(SVMModel, RangerModel)
)


for (name in names(models)) {
  
  test_that("SelectedRecipes training", {
    skip_if_not(TEST_TRAINING)
    context(paste0(name, ": Selected Recipe"))
    with_parallel({
      expect_is(fit(sel_rec, model = models[[name]]), "MLModelFit")
    })
  })
  
  test_that("SelectedRecipes training", {
    skip_if_not(TEST_TRAINING)
    context(paste0(name, ": Tuned Recipe"))
    with_parallel({
      expect_is(fit(tun_rec, model = models[[name]]), "MLModelFit")
    })
  })
  
}
