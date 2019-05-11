setOldClass("ModelFrame")
setOldClass("ModelRecipe")
setOldClass("SurvEvents")
setOldClass("SurvMatrix")
setOldClass("SurvProbs")


setClass("MLControl",
  slots = c(times = "ANY",
            dist = "ANY",
            method = "ANY",
            seed = "numeric")
)


setClass("MLControlBoot",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


setClass("MLControlCV",
  slots = c(folds = "numeric",
            repeats = "numeric"),
  contains = "MLControl"
)


setClass("MLControlOOB",
  slots = c(samples = "numeric"),
  contains = "MLControl"
)


setClass("MLControlSplit",
  slots = c(prop = "numeric"),
  contains = "MLControl"
)


setClass("MLControlTrain",
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
            types = "character",
            params = "list",
            grid = "function",
            design = "character",
            fit = "function",
            fitbits = "MLFitBits")
)


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
