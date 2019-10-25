setOldClass("ModelFrame")
setOldClass(c("ModelRecipe", "recipe"))
setOldClass("Surv")
setOldClass(c("SurvEvents", "SurvMatrix"))
setOldClass(c("SurvProbs", "SurvMatrix"))
setOldClass(c("tbl_df", "tbl", "data.frame"))


setClass("MLControl",
  slots = c(times = "ANY",
            dist = "ANY",
            method = "ANY",
            seed = "numeric")
)


setClass("MLBootstrapControl",
  contains = "MLControl",
  slots = c(samples = "numeric")
)


setClass("MLBootControl",
  contains = "MLBootstrapControl"
)


setClass("MLBootOptimismControl",
  contains = "MLBootstrapControl"
)


setClass("MLCrossValidationControl",
  contains = "MLControl",
  slots = c(folds = "numeric",
            repeats = "numeric")
)


setClass("MLCVControl",
  contains = "MLCrossValidationControl"
)


setClass("MLCVOptimismControl",
  contains = "MLCrossValidationControl"
)


setClass("MLOOBControl",
  contains = "MLControl",
  slots = c(samples = "numeric")
)


setClass("MLSplitControl",
  contains = "MLControl",
  slots = c(prop = "numeric")
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
  contains = "function",
  slots = c(name = "character",
            label = "character",
            maximize = "logical")
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
  contains = "VIRTUAL",
  slots = c(fitbits = "MLFitBits")
)


setClass("SVMModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMANOVAModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMBesselModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMLaplaceModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMLinearModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMPolyModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMRadialModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMSplineModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMTanhModelFit", contains = c("MLModelFit", "ksvm"))
setClass("CForestModelFit", contains = c("MLModelFit", "RandomForest"))


MLModelFunction <- setClass("MLModelFunction",
  contains = "function"
)


"MLModelFunction<-" <- function(object, value) {
  do.call(MLModelFunction, c(object, value))
}


MLModelList <- setClass("MLModelList",
  contains = "list"
)


setClass("Calibration",
  contains = "data.frame",
  slots = c(smoothed = "logical")
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
  contains = "matrix",
  slots = c(total = "numeric",
            accuracy = "numeric",
            majority = "numeric",
            kappa2 = "numeric")
)


setClass("Curves",
  contains = "data.frame",
  slots = c(metrics = "list")
)


setClass("Lift",
  contains = "Curves"
)


setClass("Performance",
  contains = "array"
)


setClass("PerformanceDiff",
  contains = "Performance",
  slots = c(model_names = "character")
)


PerformanceDiffTest <- setClass("PerformanceDiffTest",
  contains = "array",
  slots = c(adjust = "character")
)


MLModelTune <- setClass("MLModelTune",
  contains = "MLModel",
  slots = c(tune_grid = "tbl_df",
            performance = "Performance",
            selected = "list",
            metric = "MLMetric")
)


RecipeGrid <- setClass("RecipeGrid",
  contains = "tbl_df"
)


setClass("Resamples",
  contains = "data.frame",
  slots = c(control = "MLControl",
            strata = "character")
)


setClass("TunedRecipe",
  contains = "ModelRecipe",
  slots = c(grid = "RecipeGrid",
            params = "list")
)


setClass("VarImp",
  contains = "data.frame",
  slots = c(center = "numeric", scale = "numeric")
)
