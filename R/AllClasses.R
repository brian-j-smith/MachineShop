setOldClass("listof")
setOldClass("ModelFrame")
setOldClass("recipe")
setOldClass("Surv")
setOldClass(c("tbl_df", "tbl", "data.frame"))

setOldClass(c("param_grid", "tbl_df"))
setOldClass(c("param_set", "tbl_df"))
setOldClass(c("grid_random", "param_grid"))
setOldClass(c("grid_regular", "param_grid"))


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
            predict = "function",
            varimp = "function",
            x = "ANY",
            y = "ANY",
            tune = "ANY")
)


setClass("SelectedModel", contains = "MLModel")
setClass("StackedModel", contains = "MLModel")
setClass("SuperModel", contains = "MLModel")
setClass("TunedModel", contains = "MLModel")


setClass("MLModelFit",
  contains = "VIRTUAL",
  slots = c(mlmodel = "MLModel")
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


setClass("Calibration",
  contains = "data.frame",
  slots = c(smoothed = "logical")
)


ConfusionList <- setClass("ConfusionList",
  contains = c("listof", "list")
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


setClass("DiscreteVector",
  contains = "numeric",
  slots = c(min = "numeric",
            max = "numeric")
)


setClass("BinomialVector",
  contains = "DiscreteVector"
)


setClass("NegBinomialVector",
  contains = "DiscreteVector"
)


setClass("PoissonVector",
  contains = "DiscreteVector"
)


setClass("Grid",
  slots = c(length = "integer",
            random = "ANY")
)


setClass("Lift",
  contains = "Curves"
)


setClass("ModelRecipe",
  contains = "recipe"
)


setClass("ParamSet",
  contains = c("Grid", "param_set")
)


Performance <- setClass("Performance",
  contains = "array"
)


PerformanceDiff <- setClass("PerformanceDiff",
  contains = "Performance",
  slots = c(model_names = "character")
)


PerformanceDiffTest <- setClass("PerformanceDiffTest",
  contains = "array",
  slots = c(adjust = "character")
)


MLTune <- setClass("MLTune",
  slots = c(grid = "tbl_df",
            performance = "Performance",
            selected = "numeric",
            values = "numeric",
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


setClass("SelectedRecipe",
  contains = "ModelRecipe",
  slots = c(recipes = "list",
            params = "list")
)


setClass("SurvMatrix",
  contains = "matrix",
  slots = c(times = "numeric")
)


setClass("SurvEvents",
  contains = "SurvMatrix"
)


setClass("SurvProbs",
  contains = "SurvMatrix"
)


setClass("TunedRecipe",
  contains = "ModelRecipe",
  slots = c(grid = "RecipeGrid",
            params = "list")
)


setClass("VarImp",
  contains = "data.frame",
  slots = c(shift = "numeric",
            scale = "numeric")
)
