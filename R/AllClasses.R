setOldClass("ModelFrame")
setOldClass("ModelRecipe")
setOldClass("Surv")
setOldClass(c("SurvEvents", "SurvMatrix"))
setOldClass(c("SurvProbs", "SurvMatrix"))


setClass("MLControl",
  slots = c(times = "ANY",
            dist = "ANY",
            method = "ANY",
            seed = "numeric")
)


setClass("MLBootControl",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


setClass("MLBootOptimismControl",
  contains = "MLBootControl"
)


setClass("MLCVControl",
  slots = c(folds = "numeric",
            repeats = "numeric"),
  contains = "MLControl"
)


setClass("MLOOBControl",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


setClass("MLSplitControl",
  slots = c(prop = "numeric"),
  contains = "MLControl"
)


setClass("MLTrainControl",
  contains = "MLControl"
)


setClass("MLFitBits",
  slots = c(packages = "character",
            predict = "function",
            varimp = "function",
            x = "ANY",
            y = "ANY")
)


setClass("MLMetric",
  slots = c(name = "character",
            label = "character",
            maximize = "logical"),
  contains = "function"
)


setClass("MLModel",
  slots = c(name = "character",
            label = "character",
            packages = "character",
            response_types = "character",
            predictor_encoding = "character",
            params = "list",
            grid = "function",
            fit = "function",
            fitbits = "MLFitBits")
)


setClass("SelectedModel", contains = "MLModel")
setClass("StackedModel", contains = "MLModel")
setClass("SuperModel", contains = "MLModel")
setClass("TunedModel", contains = "MLModel")


setClass("MLModelFit",
  slots = c(fitbits = "MLFitBits"),
  contains = "VIRTUAL"
)


setClass("SVMModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMANOVAModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMBesselModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMLaplaceModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMLinearModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMPolyModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMRadialModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMSplineModelFit", contain = c("MLModelFit", "ksvm"))
setClass("SVMTanhModelFit", contain = c("MLModelFit", "ksvm"))
setClass("CForestModelFit", contains = c("MLModelFit", "RandomForest"))


setClass("Calibration",
  slots = c(smoothed = "logical"),
  contains = "data.frame"
)


setClass("ConfusionMatrix",
  contains = c("table", "matrix")
)


setClass("BinaryConfusionMatrix",
  contains = "ConfusionMatrix"
)


setClass("OrderedConfusionMatrix",
  contains = "ConfusionMatrix"
)


setClass("OrderedBinaryConfusionMatrix",
  contains = c("OrderedConfusionMatrix", "BinaryConfusionMatrix")
)


ConfusionSummary <- setClass("ConfusionSummary",
  slots = c(N = "numeric",
            Accuracy = "numeric",
            Majority = "numeric",
            Kappa = "numeric"),
  contains = "matrix"
)


setClass("Curves",
  slots = c(metrics = "list"),
  contains = "data.frame"
)


setClass("Lift",
  contains = "Curves"
)


setClass("Performance",
  contains = "array"
)


setClass("PerformanceDiff",
  slots = c(model_names = "character"),
  contains = "Performance"
)


PerformanceDiffTest <- setClass("PerformanceDiffTest",
  slots = c(adjust = "character"),
  contains = "array"
)


setClass("MLModelTune",
  slots = c(tune_grid = "data.frame",
            performance = "Performance",
            selected = "numeric"),
  contains = "MLModel"
)


setClass("Resamples",
  slots = c(control = "MLControl",
            strata = "character"),
  contains = "data.frame"
)


setClass("VarImp",
  slots = c(center = "numeric", scale = "numeric"),
  contains = "data.frame"
)
