context("Metric and Model Information")


modelinfo_names <- function(...) names(modelinfo(...))
metricinfo_names <- function(...) names(metricinfo(...))


test_that("binary outcomes", {
  skip_if_not(TEST_ALL)
  with_parallel({
    
    library(MASS)
    
    df <- Pima.tr
    fo <- type ~ .
    model_fit <- fit(fo, df, model = GBMModel)
    obs <- response(model_fit)
    pred_prob <- predict(model_fit, type = "prob")
    res <- resample(fo, df, model = GBMModel)
    
    expect_setequal(metricinfo_names(obs), metricinfo_names(factor(1:2)))
    expect_setequal(metricinfo_names(res), metricinfo_names(obs, pred_prob))
    expect_setequal(metricinfo_names(res, accuracy, auc, r2),
                    c("accuracy", "auc"))
    expect_setequal(metricinfo_names(res, "accuracy", "auc", "r2"),
                    c("accuracy", "auc"))
    expect_setequal(metricinfo_names(res, accuracy, "auc", r2),
                    c("accuracy", "auc"))

    expect_setequal(modelinfo_names(obs), modelinfo_names(factor(1:2)))
    expect_setequal(modelinfo_names(obs, CoxModel, GBMModel, GLMModel),
                    c("GBMModel", "GLMModel"))
    expect_setequal(modelinfo_names(obs, "CoxModel", "GBMModel", "GLMModel"),
                    c("GBMModel", "GLMModel"))
    expect_setequal(modelinfo_names(obs, CoxModel, "GBMModel", GLMModel),
                    c("GBMModel", "GLMModel"))
    
  })
})


test_that("category outcomes", {
  skip_if_not(TEST_ALL)
  with_parallel({
    
    df <- iris
    fo <- Species ~ .
    model_fit <- fit(fo, df, model = GBMModel)
    obs <- response(model_fit)
    pred_prob <- predict(model_fit, type = "prob")
    res <- resample(fo, df, model = GBMModel)
    
    expect_setequal(metricinfo_names(obs), metricinfo_names(factor(1:3)))
    expect_setequal(metricinfo_names(res), metricinfo_names(obs, pred_prob))
    expect_setequal(metricinfo_names(res, accuracy, brier, r2),
                    c("accuracy", "brier"))
    expect_setequal(metricinfo_names(res, "accuracy", "brier", "r2"),
                    c("accuracy", "brier"))
    expect_setequal(metricinfo_names(res, accuracy, "brier", r2),
                    c("accuracy", "brier"))
    
    expect_setequal(modelinfo_names(obs), modelinfo_names(factor(1:3)))
    expect_setequal(modelinfo_names(obs, CoxModel, GBMModel, GLMNetModel),
                    c("GBMModel", "GLMNetModel"))
    expect_setequal(modelinfo_names(obs, "CoxModel", "GBMModel", "GLMNetModel"),
                    c("GBMModel", "GLMNetModel"))
    expect_setequal(modelinfo_names(obs, CoxModel, "GBMModel", GLMNetModel),
                    c("GBMModel", "GLMNetModel"))
    
  })
})


test_that("numeric outcomes", {
  skip_if_not(TEST_ALL)
  with_parallel({
    
    df <- ICHomes
    fo <- sale_amount ~ .
    model_fit <- fit(fo, df, model = GBMModel)
    obs <- response(model_fit)
    pred <- predict(model_fit)
    res <- resample(fo, df, model = GBMModel)
    
    expect_setequal(metricinfo_names(obs), metricinfo_names(numeric(0)))
    expect_setequal(metricinfo_names(res), metricinfo_names(obs, pred))
    expect_setequal(metricinfo_names(res, mse, r2, accuracy), c("mse", "r2"))
    expect_setequal(metricinfo_names(res, "mse", "r2", "accuracy"),
                    c("mse", "r2"))
    expect_setequal(metricinfo_names(res, mse, "r2", accuracy), c("mse", "r2"))
    
    expect_setequal(modelinfo_names(obs), modelinfo_names(numeric(0)))
    expect_setequal(modelinfo_names(obs, CoxModel, GBMModel, GLMModel),
                    c("GBMModel", "GLMModel"))
    expect_setequal(modelinfo_names(obs, "CoxModel", "GBMModel", "GLMModel"),
                    c("GBMModel", "GLMModel"))
    expect_setequal(modelinfo_names(obs, CoxModel, "GBMModel", GLMModel),
                    c("GBMModel", "GLMModel"))
    
  })
})


test_that("survival outcomes", {
  skip_if_not(TEST_ALL)
  with_parallel({
    
    library(MASS)
    
    df <- Melanoma
    fo <- Surv(time, status != 2) ~ sex + age + year + thickness + ulcer
    model_fit <- fit(fo, df, model = GBMModel)
    obs <- response(model_fit)
    pred <- predict(model_fit)
    pred_times <- predict(model_fit, times = 365 * c(2, 5, 10), type = "prob")
    res <- resample(fo, df, model = GBMModel)
    
    expect_setequal(metricinfo_names(obs, pred),
                    metricinfo_names(Surv(0), numeric(0)))
    expect_setequal(metricinfo_names(obs, pred_times),
                    metricinfo_names(Surv(0), SurvProbs(0)))
    expect_setequal(metricinfo_names(res), metricinfo_names(obs, pred))
    expect_setequal(metricinfo_names(res, mse, r2, accuracy), c("mse", "r2"))
    expect_setequal(metricinfo_names(res, "mse", "r2", "accuracy"),
                    c("mse", "r2"))
    expect_setequal(metricinfo_names(res, mse, "r2", accuracy), c("mse", "r2"))
    
    expect_setequal(modelinfo_names(obs), modelinfo_names(Surv(0)))
    expect_setequal(modelinfo_names(obs, CoxModel, GBMModel, GLMModel),
                    c("GBMModel", "CoxModel"))
    expect_setequal(modelinfo_names(obs, "CoxModel", "GBMModel", "GLMModel"),
                    c("GBMModel", "CoxModel"))
    expect_setequal(modelinfo_names(obs, CoxModel, "GBMModel", GLMModel),
                    c("GBMModel", "CoxModel"))
    
  })
})
