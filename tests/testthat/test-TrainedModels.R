## Trained Models


test_that("grid tuning of models", {
  skip_if_not(TEST_TRAINING)
  with_parallel({

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
      "RFSRCModel",
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
      fit(type ~ ., data = Pima.tr,
          model = TunedModel(model, grid = 1, control = control))
    }

    test_tune_grid2 <- function(model) {
      fit(type ~ ., data = Pima.tr,
          model = TunedModel(model, grid = 3, control = control))
    }

    test_tune_grid3 <- function(model) {
      fit(type ~ ., data = Pima.tr,
          model = TunedModel(model, grid = TuningGrid(size = 100, random = 10),
                             control = control))
    }

    for (model in models) {
      expect_is(test_tune_grid1(model), "MLModelFit")
      expect_is(test_tune_grid2(model), "MLModelFit")
      expect_is(test_tune_grid3(model), "MLModelFit")
    }

  })
})
