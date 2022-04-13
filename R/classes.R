#################### Base Classes ####################


setOldClass("listof")
setOldClass(c("tbl_df", "tbl", "data.frame"))
setOldClass(c("xtabs", "table"))


ListOf <- setClass("ListOf",
  contains = c("listof", "list")
)


TabularArray <- setClass("TabularArray",
  contains = "array"
)


setClassUnion("IntegerOrLogical",
  members = c("integer", "logical")
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
  slots = c(
    times = "numeric",
    distr = "character"
  )
)


setClass("SurvEvents",
  contains = "SurvMatrix"
)


setClass("SurvProbs",
  contains = "SurvMatrix"
)


setClass("SurvTimes",
  contains = "numeric",
  slots = c(distr = "character")
)


setClass("SurvMeans", contains = "SurvTimes")


#################### Resampling Controls ####################


setClass("MLControl",
  slots = c(
    label = "character",
    monitor = "list",
    predict = "list",
    strata = "list",
    weights = "logical",
    seed = "numeric"
  )
)


setClass("BootControl",
  contains = "MLControl",
  slots = c(samples = "integer")
)


setClass("BootOptimismControl",
  contains = "BootControl"
)


setClass("CVControl",
  contains = "MLControl",
  slots = c(
    folds = "integer",
    repeats = "integer"
  )
)


setClass("CVOptimismControl",
  contains = "CVControl"
)


setClass("NullControl", contains = "MLControl")


setClass("OOBControl",
  contains = "MLControl",
  slots = c(samples = "integer")
)


setClass("SplitControl",
  contains = "MLControl",
  slots = c(prop = "numeric")
)


setClass("TrainControl",
  contains = "MLControl"
)


#################### Parameters and Grids ####################


setOldClass(c("parameters", "tbl_df"))


setClass("MLOptimization",
  slots = c(
    label = "character",
    packages = "character",
    params = "list",
    monitor = "list",
    fun = "function"
  )
)


setClass("GridSearch", contains = "MLOptimization")
setClass("NullOptimization", contains = "MLOptimization")

setClass("RandomGridSearch",
  contains = "GridSearch",
  slots = c(size = "integer")
)

setClass("SequentialOptimization",
  contains = "MLOptimization",
  slots = c(random = "IntegerOrLogical")
)


setClass("TrainingParams",
  slots = c(
    optim = "MLOptimization",
    control = "MLControl",
    metrics = "ANY",
    cutoff = "numeric",
    stat = "function",
    options = "list"
  )
)


setClassUnion("Params",
  members = c("list", "TrainingParams")
)


setClass("TuningGrid",
  slots = c(
    size = "integer",
    random = "IntegerOrLogical"
  )
)


setClass("ParameterGrid",
  contains = c("TuningGrid", "parameters")
)


RecipeGrid <- setClass("RecipeGrid",
  contains = "tbl_df"
)


setClassUnion("Grid",
  members = c("tbl_df", "TuningGrid")
)


#################### Model Components ####################


setClass("EnsembleInputOrModel",
  contains = "VIRTUAL",
  slots = c(
    candidates = "ListOf",
    params = "TrainingParams"
  )
)


setClass("MLInput",
  slots = c(
    id = "character",
    params = "ANY"
  )
)


setClass("MLModel",
  slots = c(
    id = "character",
    name = "character",
    label = "character",
    packages = "character",
    response_types = "character",
    weights = "logical",
    predictor_encoding = "character",
    params = "Params",
    gridinfo = "tbl_df",
    fit = "function",
    predict = "function",
    varimp = "function",
    steps = "ListOf"
  )
)


#################### Model Inputs ####################


setOldClass("data.frame")
setOldClass("recipe")
setOldClass(c("terms", "formula"))


setClass("ModelFrame",
  contains = c("data.frame", "MLInput")
)


setClass("ModelTerms",
  contains = "terms",
  slots = c(id = "character")
)


ModelDesignTerms <- setClass("ModelDesignTerms",
  contains = "ModelTerms"
)


ModelFormulaTerms <- setClass("ModelFormulaTerms",
  contains = "ModelTerms"
)


setClass("ModelRecipe",
  contains = c("recipe", "MLInput")
)


setClass("ModelSpecification",
  slots = c(
    id = "character",
    input = "MLInput",
    model = "MLModel",
    params = "TrainingParams",
    grid = "tbl_df"
  )
)


setClass("ModeledInput",
  contains = "VIRTUAL",
  slots = c(model = "MLModel")
)


setClass("ModeledFrame",
  contains = c("ModeledInput", "ModelFrame")
)


setClass("ModeledRecipe",
  contains = c("ModeledInput", "ModelRecipe")
)


setClass("SelectedInput",
  contains = c("EnsembleInputOrModel", "MLInput")
)


setClass("SelectedModelFrame",
  contains = c("SelectedInput", "EnsembleInputOrModel", "ModelFrame")
)


setClass("SelectedModelRecipe",
  contains = c("SelectedInput", "EnsembleInputOrModel", "ModelRecipe")
)


setClass("SelectedModelSpecification",
  contains = c("SelectedInput", "EnsembleInputOrModel", "ModelSpecification")
)


setClass("TunedInput",
  contains = "VIRTUAL",
  slots = c(
    grid = "Grid",
    params = "TrainingParams"
  )
)


setClass("TunedModelRecipe",
  contains = c("TunedInput", "ModelRecipe"),
  slots = c(grid = "RecipeGrid")
)


#################### Models ####################


setClass("EnsembleModel", contains = c("EnsembleInputOrModel", "MLModel"))
setClass("NullModel", contains = "MLModel")
setClass("ParsnipModel", contains = "MLModel")
setClass("SelectedModel", contains = "EnsembleModel")
setClass("StackedModel", contains = "EnsembleModel")
setClass("SuperModel", contains = "StackedModel")


setClass("TunedModel",
  contains = "MLModel",
  slots = c(
    model = "MLModel",
    grid = "Grid",
    params = "TrainingParams"
  )
)


setClass("MLModelFunction",
  contains = "function"
)


#################### Model Fits ####################


setClass("MLModelFit",
  contains = "VIRTUAL",
  slots = c(.MachineShop = "list")
)


setClass("CForestModelFit", contains = c("MLModelFit", "RandomForest"))
setClass("SVMModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMANOVAModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMBesselModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMLaplaceModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMLinearModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMPolyModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMRadialModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMSplineModelFit", contains = c("MLModelFit", "ksvm"))
setClass("SVMTanhModelFit", contains = c("MLModelFit", "ksvm"))


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


setClass("Performance",
  contains = "TabularArray",
  slots = c(control = "MLControl")
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
  slots = c(
    metrics = "list",
    control = "MLControl"
  )
)


setClass("LiftCurve",
  contains = "PerformanceCurve"
)


setClass("Resample",
  contains = "data.frame",
  slots = c(
    control = "MLControl",
    vars = "tbl_df"
  )
)


setClass("TrainingStep",
  slots = c(
    id = "character",
    name = "character",
    method = "character",
    log = "tbl_df",
    performance = "Performance"
  )
)


setClass("VariableImportance",
  contains = "data.frame",
  slots = c(scale = "numeric")
)
