models <- c(
  "AdaBagModel",
  "AdaBoostModel",
  "BARTMachineModel",
  "BlackBoostModel",
  "C50Model",
  "CForestModel",
  "EarthModel",
  "FDAModel",
  "GAMBoostModel",
  "GBMModel",
  "GLMBoostModel",
  "GLMNetModel",
  "KNNModel",
  "LDAModel",
  "MDAModel",
  "NNetModel",
  "PLSModel",
  "RandomForestModel",
  "RangerModel",
  "RPartModel",
  "SVMANOVAModel",
  "SVMBesselModel",
  "SVMLaplaceModel",
  "SVMLinearModel",
  "SVMPolyModel",
  "SVMRadialModel",
  "SVMTanhModel",
  "XGBDARTModel",
  "XGBLinearModel",
  "XGBTreeModel"
)

control <- CVControl(folds = 3)

test_tune_grid1 <- function(model) {
  tune(type ~ ., data = Pima.tr, model = model, grid = 1, control = control)
}

test_tune_grid2 <- function(model) {
  tune(type ~ ., data = Pima.tr, model = model, grid = 3, control = control)
}

test_tune_grid3 <- function(model) {
  tune(type ~ ., data = Pima.tr, model = model,
       grid = Grid(length = 100, random = 25), control = control)
}

for (model in models) {

  context(paste("Model Tuning:", model))

  test_that("tune grid", {
    skip_if_not(TEST_MODEL_TUNING)
    with_parallel({
      expect_is(test_tune_grid1(model), "MLModelTune")
      expect_is(test_tune_grid2(model), "MLModelTune")
      expect_is(test_tune_grid3(model), "MLModelTune")
    })
  })
  
}
