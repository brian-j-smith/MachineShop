# News

## Version Updates

## 0.3
* Implement linear models (``LMModel``), linear discriminant analysis (``LDAModel``), and quadratic discriminant analysis (``QDAModel``).
* Implement confusion matrices.
* Support matrix response variables.
* Support user-specified stratification variables for resampling via the ``strata`` argument of ``ModelFrame`` or the role of ``"case_strata"`` for recipe variables. 
* Support user-specified case weights for model fitting via the role of ``"case_weight"`` for recipe variables.
* Provide fallback for models with undefined variable importance.
* #2: Update the importing of ``prepper`` due to its relocation from ``rsample`` to ``recipes``.

## 0.2

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
