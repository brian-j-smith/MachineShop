#################### Data Structures ####################


setOldClass("listof")
setOldClass(c("tbl_df", "tbl", "data.frame"))


ListOf <- setClass("ListOf",
  contains = c("listof", "list")
)


TabularArray <- setClass("TabularArray",
  contains = "array"
)


#################### Response Variables ####################


setOldClass("BinomialVariate")
setOldClass("Surv")


setClass("DiscreteVariate",
  contains = "numeric",
  slots = c(
    min = "numeric",
    max = "numeric"
  )
)


setClass("NegBinomialVariate",
  contains = "DiscreteVariate"
)


setClass("PoissonVariate",
  contains = "DiscreteVariate"
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


#################### Tuning Grids ####################


setOldClass(c("parameters", "tbl_df"))
setOldClass(c("param_grid", "tbl_df"))
setOldClass(c("grid_random", "param_grid"))
setOldClass(c("grid_regular", "param_grid"))


setClass("Grid",
  slots = c(
    size = "integer",
    random = "ANY"
  )
)


setClass("ParameterGrid",
  contains = c("Grid", "parameters")
)


RecipeGrid <- setClass("RecipeGrid",
  contains = "tbl_df"
)


#################### Models ####################


setClass("MLModel",
  slots = c(
    name = "character",
    label = "character",
    packages = "character",
    response_types = "character",
    predictor_encoding = "character",
    params = "list",
    gridinfo = "tbl_df",
    fit = "function",
    predict = "function",
    varimp = "function",
    x = "ANY",
    train_steps = "ListOf"
  )
)


setClass("SelectedModel", contains = "MLModel")
setClass("StackedModel", contains = "MLModel")
setClass("SuperModel", contains = "MLModel")
setClass("TunedModel", contains = "MLModel")


MLModelFunction <- setClass("MLModelFunction",
  contains = "function"
)


"MLModelFunction<-" <- function(object, value) {
  do.call(MLModelFunction, c(object, value))
}


#################### Model Inputs ####################


setOldClass(c("ModelFrame", "data.frame"))
setOldClass("recipe")
setOldClass(c("terms", "formula"))


setClass("ModelTerms",
  contains = "terms"
)


ModelDesignTerms <- setClass("ModelDesignTerms",
  contains = "ModelTerms"
)


ModelFormulaTerms <- setClass("ModelFormulaTerms",
  contains = "ModelTerms"
)


setClass("ModelRecipe",
  contains = "recipe"
)


setClass("ModeledInput",
  contains = "VIRTUAL",
  slots = c(model = "MLModel")
)


setClass("ModeledTerms",
  contains = c("ModeledInput", "ModelTerms")
)


setClass("ModeledDesignTerms",
  contains = c("ModeledTerms", "ModelDesignTerms")
)


setClass("ModeledFormulaTerms",
  contains = c("ModeledTerms", "ModelFormulaTerms")
)


setClass("ModeledFrame",
  contains = c("ModeledInput", "ModelFrame")
)


setClass("ModeledRecipe",
  contains = c("ModeledInput", "ModelRecipe")
)


setClass("SelectedInput",
  contains = "VIRTUAL",
  slots = c(
    inputs = "ListOf",
    params = "list"
  )
)


setClass("SelectedModelFrame",
  contains = c("SelectedInput", "ModelFrame")
)


setClass("SelectedModelRecipe",
  contains = c("SelectedInput", "ModelRecipe")
)


setClass("TunedInput",
  contains = "VIRTUAL",
  slots = c(
    grid = "ANY",
    params = "list"
  )
)


setClass("TunedModelRecipe",
  contains = c("TunedInput", "ModelRecipe"),
  slots = c(grid = "RecipeGrid")
)


setClass("TunedModeledRecipe",
  contains = c("TunedModelRecipe", "ModeledRecipe")
)


#################### Model Fits ####################


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


#################### Resampling Controls ####################


setClass("MLControl",
  slots = c(
    strata_breaks = "integer",
    strata_nunique = "integer",
    strata_prop = "numeric",
    strata_size = "integer",
    times = "ANY",
    distr = "ANY",
    method = "ANY",
    seed = "numeric"
  )
)


setClass("MLBootstrapControl",
  contains = "MLControl",
  slots = c(samples = "integer")
)


setClass("MLBootControl",
  contains = "MLBootstrapControl"
)


setClass("MLBootOptimismControl",
  contains = "MLBootstrapControl"
)


setClass("MLCrossValidationControl",
  contains = "MLControl",
  slots = c(
    folds = "integer",
    repeats = "integer"
  )
)


setClass("MLCVControl",
  contains = "MLCrossValidationControl"
)


setClass("MLCVOptimismControl",
  contains = "MLCrossValidationControl"
)


setClass("MLOOBControl",
  contains = "MLControl",
  slots = c(samples = "integer")
)


setClass("MLSplitControl",
  contains = "MLControl",
  slots = c(prop = "numeric")
)


setClass("MLTrainControl",
  contains = "MLControl"
)


#################### Performance Measures ####################


setClass("Calibration",
  contains = "data.frame",
  slots = c(smoothed = "logical")
)


ConfusionList <- setClass("ConfusionList",
  contains = "ListOf"
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
  slots = c(
    total = "numeric",
    accuracy = "numeric",
    majority = "numeric",
    kappa2 = "numeric"
  )
)


setClass("MLMetric",
  contains = "function",
  slots = c(
    name = "character",
    label = "character",
    maximize = "logical"
  )
)


Performance <- setClass("Performance",
  contains = "TabularArray"
)


PerformanceDiff <- setClass("PerformanceDiff",
  contains = "Performance",
  slots = c(model_names = "character")
)


PerformanceDiffTest <- setClass("PerformanceDiffTest",
  contains = "TabularArray",
  slots = c(adjust = "character")
)


setClass("PerformanceCurve",
  contains = "data.frame",
  slots = c(metrics = "list")
)


setClass("LiftCurve",
  contains = "PerformanceCurve"
)


setClass("Resamples",
  contains = "data.frame",
  slots = c(
    control = "MLControl",
    strata = "character"
  )
)


TrainStep <- setClass("TrainStep",
  slots = c(
    grid = "tbl_df",
    performance = "Performance",
    selected = "numeric",
    values = "numeric",
    metric = "MLMetric"
  )
)


setClass("VarImp",
  contains = "data.frame",
  slots = c(
    shift = "numeric",
    scale = "numeric"
  )
)
