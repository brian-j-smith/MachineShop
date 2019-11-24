context("Combine Methods")


test_that("object combinations", {
  skip_if_not(TEST_ALL)
  with_parallel({
    
    library(MASS)
    
    df <- Pima.tr
    fo <- type ~ .
    
    model <- GLMModel
    
    
    model_fit <- fit(fo, data = df, model = model)
    res <- resample(fo, data = df, model = model)
    
    cal <- calibration(res)
    conf <- confusion(res)
    conf_summary <- summary(conf)
    conf_perf <- performance(conf)
    curve <- performance_curve(res)
    lift_curve <- lift(res)
    
    expect_s4_class(c(cal), "Calibration")
    expect_s4_class(c(cal, cal), "Calibration")
    expect_type(c(cal, res), "list")
    
    expect_s4_class(c(conf), "ConfusionList")
    expect_s4_class(c(conf, conf), "ConfusionList")
    expect_s4_class(c(conf, conf[[1]]), "ConfusionList")
    expect_type(c(conf, res), "list")
    expect_s4_class(c(conf[[1]]), "ConfusionList")
    expect_s4_class(c(conf[[1]], conf[[1]]), "ConfusionList")
    expect_s4_class(c(conf[[1]], conf), "ConfusionList")
    expect_type(c(conf[[1]], res), "list")
    
    expect_s4_class(c(curve), "Curves")
    expect_s4_class(c(curve, curve), "Curves")
    expect_type(c(curve, res), "list")
    expect_error(c(curve, lift_curve))
    expect_s4_class(c(lift_curve), "Curves")
    expect_s4_class(c(lift_curve, lift_curve), "Curves")
    expect_type(c(lift_curve, curve), "list")
    
    expect_s4_class(c(conf_summary), "ListOf")
    expect_s4_class(c(conf_summary, conf_summary), "ListOf")
    expect_type(c(conf_summary, conf_perf), "list")
    expect_type(c(conf_summary, res), "list")
    expect_s4_class(c(conf_perf), "ListOf")
    expect_s4_class(c(conf_perf, conf_perf), "ListOf")
    expect_type(c(conf_perf, conf_summary), "list")
    expect_type(c(conf_perf, res), "list")
    
    expect_s4_class(c(res), "Resamples")
    expect_s4_class(c(res, res), "Resamples")
    expect_type(c(res, cal), "list")
    expect_type(c(res, conf), "list")
    expect_type(c(res, conf[[1]]), "list")
    expect_type(c(res, conf_summary), "list")
    expect_type(c(res, conf_perf), "list")
    expect_type(c(res, curve), "list")
    expect_type(c(res, lift_curve), "list")
    
    x <- runif(10, 0, 100)
    binom <- BinomialMatrix(x, size = 100)
    disc <- DiscreteVector(x)
    negbinom <- NegBinomialVector(x)
    pois <- PoissonVector(x)
    
    expect_s3_class(c(binom), "BinomialMatrix")
    expect_s3_class(c(binom, binom), "BinomialMatrix")
    expect_type(c(binom, disc), "integer")
    expect_type(c(binom, negbinom), "integer")
    expect_type(c(binom, pois), "integer")
    expect_type(c(binom, res), "list")
    expect_s4_class(c(disc), "DiscreteVector")
    expect_type(c(disc, binom), "integer")
    expect_s4_class(c(disc, disc), "DiscreteVector")
    expect_s4_class(c(disc, negbinom), "DiscreteVector")
    expect_s4_class(c(disc, pois), "DiscreteVector")
    expect_type(c(disc, res), "list")
    expect_s4_class(c(negbinom), "NegBinomialVector")
    expect_type(c(negbinom, binom), "integer")
    expect_type(c(negbinom, disc), "integer")
    expect_s4_class(c(negbinom, negbinom), "NegBinomialVector")
    expect_type(c(negbinom, pois), "integer")
    expect_type(c(negbinom, res), "list")
    expect_s4_class(c(pois), "PoissonVector")
    expect_type(c(pois, binom), "integer")
    expect_type(c(pois, disc), "integer")
    expect_type(c(pois, negbinom), "integer")
    expect_s4_class(c(pois, pois), "PoissonVector")
    expect_type(c(pois, res), "list")
    
    x <- DiscreteVector(1:10, 1, 10)
    y <- DiscreteVector(-(10:1), -10, -1)
    z <- expect_s4_class(c(x, y), "DiscreteVector")
    expect_equal(length(z), length(x) + length(y))
    expect_equal(z@min, min(x@min, y@min))
    expect_equal(z@max, max(x@max, y@max))
    
  })
})
