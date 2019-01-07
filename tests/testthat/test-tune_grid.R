models <- c(
  "AdaBagModel",
  "AdaBoostModel",
  "BARTMachineModel",
  "BlackBoostModel",
  "C50Model",
  "CForestModel",
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

test_tune_grid <- function(model) {
  fo <- type ~ .
  df <- Pima.tr
  tune(fo, data = df, grid = 1, model = model)
  tune(fo, data = df, grid = 3, model = model)
}

for (model in models) {

  context(paste("Model Tuning:", model))

  test_that("tune grid", {
    skip_if_not(TEST_MODEL_TUNING)
    with_parallel({
      expect_is(test_tune_grid(model), "MLModelTune")
    })
  })
  
}
