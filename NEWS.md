# News

## Version Updates

## 2.1.3
* Revisions needed to some `fit()` methods to ensure that unprepped recipes are passed to models, like `TunedModed`, `StackedModel`, `SelectedModel` and `SuperModel`, needing to replicate preprocessing steps in their resampling routines.
* Extend `GLMModel` to factor and matrix responses.
* Use `fun` instead of deprecated `fun.y` in ggplot2 functions.
* Capture user-supplied parameters passed in to the ellipsis of model constructor functions that have them.


## 2.1.2
* Compatibility fix for tibble 3.0.0.
* Include missing values in model matrices created internally from formulas.


## 2.1.1
* Improve specificity of `metricinfo()` results for factor responses.
* Correct `SplitControl()` to train on the split sample instead of the full dataset.
* Perform stratified resampling automatically when `fit()` formula and matrix methods are called with meta-models.


## 2.1.0
* CRAN release.


## 2.0.4
* Extend `print()` argument `n` to data frame and matrix columns for more concise display of large data structures.
* Add preprocessing recipe functions `step_kmeans()`, `step_kmedoids()`, and `step_spca()`.


## 2.0.3
* Internal changes:
  * Remove `MLModel` slot `y`.
  * Rename `ModelFrame` and `ModelRecipe` columns `(casenames)` to `(names)`.
  * Register `ModelFrame` inheritance from `data.frame`.
  * Define `Terms` S4 classes for `ModelFrame` slot `terms`.


## 2.0.2
* Implement `ModeledInput`, `SelectedInput` and `TunedInput` classes and methods.
* Deprecate `SelectedFormula()`, `SelectedMatrix()`, `SelectedModelFrame()`, `SelectedRecipe()`, and `TunedRecipe()`.
* Remove deprecated `tune()`.
* Rename global setting `stat.Curves` to `stat.Curve`.


## 2.0.1
* Rename global setting `stat.Train` to `stat.train`.
* Add print methods for `SelectedModel`, `StackedModel`, `SuperModel`, and `TunedModel`.
* Revise training methods to ensure nested resampling of `SelectedRecipe` and `TunedRecipe`.
* Return list of all training steps in `MLModel` `trainbits` slot.


## 2.0.0
* Rename global setting `stat.Tune` to `stat.Train`.
* Enable selection of formulas, design matrices, and model frames with `SelectedFormula()`, `SelectedMatrix()`, and `SelectedModelFrame()`.
* Rename discrete variable classes: `BinomialMatrix` &rarr; `BinomialVariate`, `DiscreteVector` &rarr; `DiscreteVariate`, `NegBinomialVector` &rarr; `NegBinomialVariate`, and `PoissonVector` &rarr; `PoissonVariate`.
* Add global setting `require` for user-specified packages to load during parallel execution of resampling algorithms.
* Rename recipe role `case_strata` to `case_stratum`.
* Rename `object` argument to `data` in `ConfusionMatrix()`, `SurvEvents()`, and `SurvProbs()`.
* Add `c` methods for `BinomialVariate`, `DiscreteVariate`, `ListOf`, and `SurvMatrix`.
* Add `role_binom()`, `role_case()`, `role_surv()`, and `role_term()` to set recipe roles.
* Support `base` argument to `varimp()` for log-transformed p-values.
* Rename `ParamSet` to `ParameterGrid`.
* Add option to `reset` global settings individually.
* Add `as.data.frame` methods for `Performance`, `Performance` summary, `PerformanceDiff`, `PerformanceDiffTest`, and `Resamples`.


## 1.99.0
* Implement `DiscreteVector` class and subclasses `BinomialVector`, `NegBinomialVector`, and `PoissonVector` for discrete response variables.
* Extend model support to `DiscreteVector` classes as follows.
  * `DiscreteVector`: all models applicable to numeric responses.
  * `BinomialVector`/`NegBinomialVector`/`PoissonVector`: `BlackBoostModel`, `GAMBoostModel`, `GLMBoostModel`, `GLMModel`, and `GLMStepAICModel`.
  * `BinomialVector`/`PoissonVector`: `GLMNetModel`.
  * `PoissonVector`: `GBMModel` and `XGBModel`
* Add support for offset terms in formulas, model matrices, and recipes.
* Add recipe tune information to fitted `MLModel`.
* Replace `Calibration()`, `Confusion()`, `Curves()`, `Lift()`, and `Resamples()` with `c` methods.
* Redefine `Confusion` S3 class as `ConfusionList` S4 class.
* Remove support for one-element list to `metricinfo()` and `modelinfo()`.
* Remove deprecated `expand.model()`.
* Expire deprecated `tune()`.


## 1.6.4
* Calculate regression variable importance as negative log p-values.
* Support empty vectors in `metricinfo()` and `modelinfo()`.
* Add support for **dials** package parameter sets with `ParamSet()`.


## 1.6.3
* Add `as.MLModel()` for coercing `MLModelFit` to `MLModel`.
* Deprecate `tune()`; call `fit()` with a `SelectedModel` or `TunedModel` instead.


## 1.6.2
* Implement optimism-corrected cross-validation (`CVOptimismControl`).
* Fix `BootOptimismControl` error with 2D responses.
* Add global option `max.print` for the number of models and data frame rows to show with print methods.
* Enable recipe selection with `SelectedRecipe()`.
* Refactor `tune()` methods.
* Replace `MLModelFit` element `fitbits` (`MLFitBits` object) with `mlmodel` (`MLModel` object).
* Rename `VarImp` slot `center` to `shift`.


## 1.6.1
* Use tibbles for parameter grids.
* Add random sampling option to `expand_model()`, `expand_params()`, and `expand_steps()`.
* Display information for model functions and objects more compactly.


## 1.6.0
* Add global setting for default cutoff threshold value.
* Add option to reset all global settings.
* Enable recipe tuning with `TunedRecipe()`.
* Add `expand_model()` for model expansion over tuning parameters.
* Add `expand_params()` for model parameters expansion.
* Add `expand_steps()` for recipe step parameters expansion.
* Implement `MLModelFunction` and `MLModelList` classes.
* Add fit methods for `MLModel`, `MLModelFunction`, and `MLModelList`.
* Fix `NNetModel` fit error with binary and factor responses.
* Fix `modelinfo()` function not found error.


## 1.5.2
* Implement exception handling of `tune()` resampling failures.
* Remove deprecated `types` and `design` arguments from `MLModel()`.


## 1.5.1
* Implement global settings for default resampling control, performance metrics, summary statistics, and tuning grid.
* Support vector arguments in `metricinfo()` and `modelinfo()`.
* Update package documentation.


## 1.5.0
* Implement model: `SelectedModel`.
* Remove `maximize` argument from `tune()` and `TunedModel`.
* Support lists as arguments to `StackedModel()` and `SuperModel`.


## 1.4.2
* Revert renaming of `expand.model()`.
* Exclude 0 distance from `KNNModel` tuning grid.
* Improve random tuning grid coverage.


## 1.4.1
* Implement model: `TunedModel`.
* Remove deprecated `na.action` argument from `ModelFrame` methods.
* Rename `MLModel()` argument `types` to `response_types`.
* Rename `MLModel()` argument `design` to `predictor_encoding`.
* Rename `expand.model()` to `expand_model()`.


## 1.4.0
* CRAN release.


## 1.3.3
* Implement optimism-corrected bootstrap resampling (`BootOptimismControl`).
* Store case names in `ModelFrame` and `ModelRecipe` and save to `Resamples`.


## 1.3.2
* Add `BinaryConfusionMatrix` and `OrderedConfusionMatrix` classes.
* Export `ConfusionMatrix` constructor.
* Extend `metricinfo()` to confusion matrices.
* Refactor performance metrics methods code.


## 1.3.1
* Check and convert ordered factors in response methods.
* Check consistency of extracted variables in response methods.
* Add metrics methods for `Resamples`.


## 1.3.0
* Improve compatibility with preprocessing recipes.
* Allow base math functions and operators in `ModelFrame` formulas.


## 1.2.5
* Save `ModelFrame` response in first column.
* Unexport `response` formula method.
* Add `ICHomes` dataset.
* Add `center` and `scale` slot to `VarImp`.


## 1.2.4
* Prohibit in-line functions in `ModelFrame` formulas.
* Rename `response` function argument from `data` to `newdata`.


## 1.2.3
* Add `fit`, `resample`, and `tune` methods for design matrices.
* Reduce computational overhead for design matrices and recipes.
* Rename `ModelFrame()` argument `na.action` to `na.rm`.


## 1.2.2
* Implement parametric (``"exponential"``, ``"rayleigh"``, ``"weibull"``) estimation of baseline survival functions.
* Set ``"weibull"`` as the default distribution for survival mean estimation.
* Add extract method for ``Resamples``.
* Add ``na.rm`` argument to ``calibration()``, ``confusion()``, ``performance()``, and ``performance_curve()``.
* Add loess ``span`` argument to ``calibration()``.
* Change ``SurvMatrix`` from S4 to S3 class.


## 1.2.1
* Add ``method`` option to ``predict()`` for Breslow, Efron (default), or Fleming-Harrington estimation of survival curves for Cox proportional hazards-based models.
* Add ``dist`` option to ``predict()`` for exponential or Weibull approximation to estimated survival curves.
* Add ``dist`` option to ``calibration()`` for distributional estimation of observed mean survival.
* Add ``dist`` option to ``r2()`` for distributional estimation of the total sum of squares mean.
* Handle unnamed arguments in ``metricinfo()`` and ``modelinfo()``.


## 1.2.0
* Implement metrics: ``auc``, ``fnr``, ``fpr``, ``rpp``, ``tnr``, ``tpr``.
* Implement performance curves, including ROC and precision recall.
* Implement ``SurvMatrix`` classes for predicted survival events and probabilities to eliminate need for separate ``times`` arguments in calibration, confusion, metrics, and performance functions.
* Add calibration curves for predicted survival means.
* Add lift curves for predicted survival probabilities.
* Add recipe support for survival and matrix outcomes.
* Rename ``MLControl`` argument ``surv_times`` to ``times``.
* Fix identification of recipe ``case_weight`` and ``case_strata`` variables.
* Launch package [website](https://brian-j-smith.github.io/MachineShop/).
* Bring Introduction vignette up to date with package features.


## 1.1.0
* Implement model: ``BARTModel``.
* Implement model tuning over automatically generated grids of parameter values and random sampling of grid points.
* Add metrics for predicted survival times: ``accuracy``, ``f_score``, ``kappa2``, ``npv``, ``ppv``, ``pr_auc``, ``precision``, ``recall``, ``roc_index``, ``sensitivity``, ``specificity``
* Add metrics for predicted survival means: ``cindex``, ``gini``, ``mae``, ``mse``, ``msle``, ``r2``, ``rmse``, ``rmsle``.
* Add ``performance`` and metric methods for ``ConfusionMatrix``.
* Add confusion matrices for predicted survival times.
* Standardize predict functions to return mean survival when times are not specified.
* Replace ``MLModel`` slot and constructor argument ``nvars`` with ``design``.


## 1.0.0

* Implement models: ``BARTMachineModel``, ``LARSModel``.
* Implement performance metrics: ``gini``, multi-class ``pr_auc`` and ``roc_auc``, multivariate ``rmse``, ``msle``, ``rmsle``.
* Implement smooth calibration curves.
* Implement ``MLMetric`` class for performance metrics.
* Add ``as.data.frame`` method for ``ModelFrame``.
* Add ``expand.model`` function.
* Add ``label`` slot to ``MLModel``.
* Expand ``metricinfo/modelinfo`` support for mixed argument types.
* Rename ``calibration`` argument ``n`` to ``breaks``.
* Rename ``modelmetrics`` function to ``performance``.
* Rename ``ModelMetrics/Diff`` classes to ``Performance/Diff``.
* Change ``MLModelTune`` slot ``resamples`` to ``performance``.


## 0.4.0
* Implement models: ``AdaBagModel``, ``AdaBoostModel``, ``BlackBoostModel``, ``EarthModel``, ``FDAModel``, ``GAMBoostModel``, ``GLMBoostModel``, ``MDAModel``, ``NaiveBayesModel``, ``PDAModel``, ``RangerModel``, ``RPartModel``, ``TreeModel``
* Implement user-specified performance metrics in ``modelmetrics`` function.
* Implement metrics: ``accuracy``, ``brier``, ``cindex``, ``cross_entropy``, ``f_score``, ``kappa2``, ``mae``, ``mse``, ``npv``, ``ppv``, ``pr_auc``, ``precision``, ``r2``, ``recall``, ``roc_auc``, ``roc_index``, ``sensitivity``, ``specificity``, ``weighted_kappa2``.
* Add ``cutoff`` argument to ``confusion`` function.
* Add ``modelinfo`` and ``metricinfo`` functions.
* Add ``modelmetrics`` method for ``Resamples``.
* Add ``ModelMetrics`` class with ``print`` and ``summary`` methods.
* Add ``response`` method for ``recipe``.
* Export ``Calibration`` constructor.
* Export ``Confusion`` constructor.
* Export ``Lift`` constructor.
* Extend ``calibration`` arguments to observed and predicted responses.
* Extend ``confusion`` arguments to observed and predicted responses.
* Extend ``lift`` arguments to observed and predicted responses.
* Extend ``metrics`` and ``stats`` function arguments to accept function names.
* Extend ``Resamples`` to arguments with multiple models.
* Change ``CoxModel``, ``GLMModel``, and ``SurvRegModel`` constructor definitions so that model control parameters are specified directly instead of with a separate ``control`` argument/structure.
* Change ``predict(..., times = numeric())`` function calls to survival model fits to return predicted values in the same direction as survival times.
* Change ``predict(..., times = numeric())`` function calls to ``CForestModel`` fits to return predicted means instead of medians.
* Change ``tune`` function argument ``metrics`` to be defined in terms of a user-specified metric or metrics.
* Deprecate MLControl arguments ``cutoff``, ``cutoff_index``, ``na.rm``, and ``summary``.


## 0.3.0
* Implement linear models (``LMModel``), linear discriminant analysis (``LDAModel``), and quadratic discriminant analysis (``QDAModel``).
* Implement confusion matrices.
* Support matrix response variables.
* Support user-specified stratification variables for resampling via the ``strata`` argument of ``ModelFrame`` or the role of ``"case_strata"`` for recipe variables. 
* Support user-specified case weights for model fitting via the role of ``"case_weight"`` for recipe variables.
* Provide fallback for models with undefined variable importance.
* Update the importing of ``prepper`` due to its relocation from ``rsample`` to ``recipes``.

## 0.2.0

* Implement partial dependence, calibration, and lift estimation and plotting.
* Implement k-nearest neighbors model (``KNNModel``), stacked regression models (``StackedModel``), super learner models (``SuperModel``), and extreme gradient boosting (``XGBModel``).
* Implement resampling constructors for training resubstitution (``TrainControl``) and split training and test sets (``SplitControl``).
* Implement ``ModelFrame`` class for general model formula and dataset specification.
* Add multi-class Brier score to ``modelmetrics()``.
* Extend ``predict()`` to automatically preprocess recipes and to use training data as the ``newdata`` default.
* Extend ``tune()`` to lists of models.
* Extent ``summary()`` argument ``stats`` to functions.
* Fix survival probability calculations in ``GBMModel`` and ``GLMNetModel``.
* Change ``MLControl`` argument ``na.rm`` default from ``FALSE`` to ``TRUE``.
* Removed ``na.rm`` argument from ``modelmetrics()``.

## 0.1

* Initial public release
