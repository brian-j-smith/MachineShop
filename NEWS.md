# News

## Version Updates

## 0.4.0
* Implement models: ``AdaBagModel``, ``AdaBoostModel``, ``BlackBoostModel``, ``EarthModel``, ``FDAModel``, ``GAMBoostModel``, ``GLMBoostModel``, ``MDAModel``, ``NaiveBayesModel``, ``PDAModel``, ``RangerModel``, ``RPartModel``, ``TreeModel``
* Implement user-specified performance metrics in ``modelmetrics`` function.
* Implement metrics: ``accuracy``, ``brier``, ``cindex``, ``cross_entropy``, ``f_score``, ``kappa2``, ``mae``, ``mse``, ``npv``, ``ppv``, ``pr_auc``, ``precision``, ``r2``, ``recall``, ``roc_auc``, ``roc_index``, ``sensitivity``, ``specificity``, ``weighted_kappa2``.
* Add ``cutoff`` argument to ``confusion`` function.
* Add ``modelinfo`` and ``metricinfo`` functions.
* Add ``modelmetrics`` method for ``Resamples``.
* Add ``ModelMetrics`` class with ``print`` and ``summary`` methods.
* Add ``response`` method for ``recipe``.
* Extend ``calibration`` arguments to observed and predicted responses.
* Extend ``confusion`` arguments to observed and predicted responses.
* Extend ``lift`` arguments to observed and predicted responses.
* Extend ``metrics`` and ``stats`` function arguments to accept function names.
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
