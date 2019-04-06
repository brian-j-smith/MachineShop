# News

## Version Updates

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
