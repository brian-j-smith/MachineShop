
# MachineShop: Machine Learning Models and Tools

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/MachineShop)](http://cran.r-project.org/web/packages/MachineShop)

## Overview

`MachineShop` is a meta-package for statistical and machine learning
with a common interface for model fitting, prediction, performance
assessment, and presentation of results. Support is provided for
predictive modeling of numerical, categorical, and censored
time-to-event outcomes, including those listed in the table below, and
for resample (bootstrap, cross-validation, and split training-test sets)
estimation of model
performance.

<div>

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="border-bottom:hidden" colspan="1">

</th>

<th style="border-bottom:hidden" colspan="1">

</th>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">

Response Variable Types

</div>

</th>

</tr>

<tr>

<th style="text-align:left;">

Method

</th>

<th style="text-align:center;">

Constructor

</th>

<th style="text-align:center;">

Categorical<sup>1</sup>

</th>

<th style="text-align:center;">

Continuous<sup>2</sup>

</th>

<th style="text-align:center;">

Survival<sup>3</sup>

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Bagging with Classification Trees

</td>

<td style="text-align:center;">

AdaBagModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Boosting with Classification Trees

</td>

<td style="text-align:center;">

AdaBoostModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Bayesian Additive Regression Trees

</td>

<td style="text-align:center;">

BARTMachineModel

</td>

<td style="text-align:center;">

b

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Gradient Boosting with Regression Trees

</td>

<td style="text-align:center;">

BlackBoostModel

</td>

<td style="text-align:center;">

b

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

C5.0 Classification

</td>

<td style="text-align:center;">

C50Model

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Conditional Random Forests

</td>

<td style="text-align:center;">

CForestModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Cox Regression

</td>

<td style="text-align:center;">

CoxModel

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Cox Regression (Stepwise)

</td>

<td style="text-align:center;">

CoxStepAICModel

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Multivariate Adaptive Regression Splines

</td>

<td style="text-align:center;">

EarthModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Flexible Discriminant Analysis

</td>

<td style="text-align:center;">

FDAModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Gradient Boosting with Additive Models

</td>

<td style="text-align:center;">

GAMBoostModel

</td>

<td style="text-align:center;">

b

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Generalized Boosted Regression

</td>

<td style="text-align:center;">

GBMModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Gradient Boosting with Linear Models

</td>

<td style="text-align:center;">

GLMBoostModel

</td>

<td style="text-align:center;">

b

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Generalized Linear Models

</td>

<td style="text-align:center;">

GLMModel

</td>

<td style="text-align:center;">

b

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Generalized Linear Models (Stepwise)

</td>

<td style="text-align:center;">

GLMStepAICModel

</td>

<td style="text-align:center;">

b

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Lasso and Elastic-Net

</td>

<td style="text-align:center;">

GLMNetModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

m, n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

K-Nearest Neighbors Model

</td>

<td style="text-align:center;">

KNNModel

</td>

<td style="text-align:center;">

f, o

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Least Angle Regression

</td>

<td style="text-align:center;">

LARSModel

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Linear Discriminant Analysis

</td>

<td style="text-align:center;">

LDAModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Linear Model

</td>

<td style="text-align:center;">

LMModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

m, n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Mixture Discriminant Analysis

</td>

<td style="text-align:center;">

MDAModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Naive Bayes Classifier

</td>

<td style="text-align:center;">

NaiveBayesModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Feed-Forward Neural Networks

</td>

<td style="text-align:center;">

NNetModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Penalized Discriminant Analysis

</td>

<td style="text-align:center;">

PDAModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Partial Least Squares

</td>

<td style="text-align:center;">

PLSModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Ordered Logistic Regression

</td>

<td style="text-align:center;">

POLRModel

</td>

<td style="text-align:center;">

o

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Quadratic Discriminant Analysis

</td>

<td style="text-align:center;">

QDAModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Random Forests

</td>

<td style="text-align:center;">

RandomForestModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Fast Random Forests

</td>

<td style="text-align:center;">

RangerModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Recursive Partitioning and Regression Trees

</td>

<td style="text-align:center;">

RPartModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Stacked Regression

</td>

<td style="text-align:center;">

StackedModel

</td>

<td style="text-align:center;">

f, o

</td>

<td style="text-align:center;">

m, n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Super Learner

</td>

<td style="text-align:center;">

SuperModel

</td>

<td style="text-align:center;">

f, o

</td>

<td style="text-align:center;">

m, n

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Parametric Survival

</td>

<td style="text-align:center;">

SurvRegModel

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Parametric Survival (Stepwise)

</td>

<td style="text-align:center;">

SurvRegStepAICModel

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

S

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines

</td>

<td style="text-align:center;">

SVMModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (ANOVA)

</td>

<td style="text-align:center;">

SVMANOVAModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (Bessel)

</td>

<td style="text-align:center;">

SVMBesselModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (Laplace)

</td>

<td style="text-align:center;">

SVMLaplaceModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (Linear)

</td>

<td style="text-align:center;">

SVMLinearModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (Poly)

</td>

<td style="text-align:center;">

SVMPolyModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (Radial)

</td>

<td style="text-align:center;">

SVMRadialModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (Spline)

</td>

<td style="text-align:center;">

SVMSplineModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Support Vector Machines (Tanh)

</td>

<td style="text-align:center;">

SVMTanhModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Regression and Classification Trees

</td>

<td style="text-align:center;">

TreeModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Extreme Gradient Boosting

</td>

<td style="text-align:center;">

XGBModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Extreme Gradient Boosting (DART)

</td>

<td style="text-align:center;">

XGBDARTModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Extreme Gradient Boosting (Linear)

</td>

<td style="text-align:center;">

XGBLinearModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Extreme Gradient Boosting (Tree)

</td>

<td style="text-align:center;">

XGBTreeModel

</td>

<td style="text-align:center;">

f

</td>

<td style="text-align:center;">

n

</td>

<td style="text-align:center;">

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<sup>1</sup> b = binary, f = factor, o = ordered

</td>

</tr>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<sup>2</sup> m = matrix, n = numeric

</td>

</tr>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<sup>3</sup> S = Surv

</td>

</tr>

</tfoot>

</table>

</div>

## Installation

``` r
# Current release from CRAN
install.packages("MachineShop")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("brian-j-smith/MachineShop", ref = "develop")

# Development version with vignettes
devtools::install_github("brian-j-smith/MachineShop", ref = "develop", build_vignettes = TRUE)
```

## Documentation

Once the package is installed, general documentation on its usage can be
viewed with the following console commands.

``` r
library(MachineShop)

# Package help summary
?MachineShop

# Vignette
RShowDoc("Introduction", package = "MachineShop")
```

## Parallel Computing

Resampling algorithms will be executed in parallel automatically if a
parallel backend for the `foreach` package, such as `doParallel`, is
loaded.

``` r
library(doParallel)
registerDoParallel(cores = 4)
```

## Example

The following is a brief example illustrating use of the package to
predict the species of flowers in Edgar Anderson’s iris data set.

### Training and Test Set Analysis

``` r
## Load the package
library(MachineShop)
library(magrittr)

## Iris flower species (3 level response) data set
head(iris)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

## Training and test sets
set.seed(123)
trainindices <- sample(nrow(iris), nrow(iris) * 2 / 3)
train <- iris[trainindices, ]
test <- iris[-trainindices, ]

## Model formula
fo <- Species ~ .

## Models by response type
modelinfo(factor(0)) %>% names
#>  [1] "AdaBagModel"       "AdaBoostModel"     "C50Model"         
#>  [4] "CForestModel"      "EarthModel"        "FDAModel"         
#>  [7] "GBMModel"          "GLMNetModel"       "KNNModel"         
#> [10] "LDAModel"          "LMModel"           "MDAModel"         
#> [13] "NaiveBayesModel"   "NNetModel"         "PDAModel"         
#> [16] "PLSModel"          "QDAModel"          "RandomForestModel"
#> [19] "RangerModel"       "RPartModel"        "StackedModel"     
#> [22] "SuperModel"        "SVMModel"          "SVMANOVAModel"    
#> [25] "SVMBesselModel"    "SVMLaplaceModel"   "SVMLinearModel"   
#> [28] "SVMPolyModel"      "SVMRadialModel"    "SVMSplineModel"   
#> [31] "SVMTanhModel"      "TreeModel"         "XGBModel"         
#> [34] "XGBDARTModel"      "XGBLinearModel"    "XGBTreeModel"

## Model-specific information
modelinfo(GBMModel)
#> $GBMModel
#> $GBMModel$label
#> [1] "Generalized Boosted Regression"
#> 
#> $GBMModel$packages
#> [1] "gbm"
#> 
#> $GBMModel$types
#> [1] "factor"  "numeric" "Surv"   
#> 
#> $GBMModel$arguments
#> function (distribution = NULL, n.trees = 100, interaction.depth = 1, 
#>     n.minobsinnode = 10, shrinkage = 0.1, bag.fraction = 0.5) 
#> NULL
#> 
#> $GBMModel$varimp
#> [1] TRUE

## Generalized boosted model fit to training set
gbmfit <- fit(fo, data = train, model = GBMModel)

## Variable importance
(vi <- varimp(gbmfit))
#>                 Overall
#> Petal.Length 100.000000
#> Petal.Width   14.601856
#> Sepal.Width    1.438558
#> Sepal.Length   0.000000

plot(vi)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
## Test set predicted probabilities
predict(gbmfit, newdata = test, type = "prob") %>% head
#>         setosa   versicolor    virginica
#> [1,] 0.9999737 2.627994e-05 4.493784e-08
#> [2,] 0.9999154 8.456383e-05 4.205211e-09
#> [3,] 0.9999154 8.456383e-05 4.205211e-09
#> [4,] 0.9999737 2.627994e-05 4.493784e-08
#> [5,] 0.9998807 1.192834e-04 2.987679e-09
#> [6,] 0.9999024 9.764241e-05 2.445639e-09

## Test set predicted classifications
predict(gbmfit, newdata = test) %>% head
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica

## Test set performance
obs <- response(fo, data = test)
pred <- predict(gbmfit, newdata = test, type = "prob")
performance(obs, pred)
#>  Accuracy     Kappa    ROCAUC     Brier 
#> 0.9200000 0.8793727 0.9837585 0.1502442
```

### Resampling

``` r
## Resample estimation of model performance
(res <- resample(fo, data = iris, model = GBMModel, control = CVControl))
#> An object of class "Resamples"
#> 
#> Models: GBMModel
#> Stratification variable: (strata) 
#> 
#> An object from class "MLControl"
#> 
#> Name: CVControl
#> Label: K-Fold Cross-Validation
#> Folds: 10
#> Repeats: 1
#> Seed: 253452646

summary(res)
#>                Mean    Median         SD          Min       Max NA
#> Accuracy 0.95333333 0.9333333 0.03220306 9.333333e-01 1.0000000  0
#> Kappa    0.93000000 0.9000000 0.04830459 9.000000e-01 1.0000000  0
#> ROCAUC   0.98800000 1.0000000 0.02305120 9.333333e-01 1.0000000  0
#> Brier    0.08476969 0.1133233 0.05621648 5.140496e-05 0.1372037  0

plot(res)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

### Performance Metrics

``` r
## Default performance metrics
performance(res) %>% summary
#>                Mean    Median         SD          Min       Max NA
#> Accuracy 0.95333333 0.9333333 0.03220306 9.333333e-01 1.0000000  0
#> Kappa    0.93000000 0.9000000 0.04830459 9.000000e-01 1.0000000  0
#> ROCAUC   0.98800000 1.0000000 0.02305120 9.333333e-01 1.0000000  0
#> Brier    0.08476969 0.1133233 0.05621648 5.140496e-05 0.1372037  0

## All available metric functions
metricinfo() %>% names
#>  [1] "accuracy"        "brier"           "cindex"         
#>  [4] "cross_entropy"   "f_score"         "gini"           
#>  [7] "kappa2"          "mae"             "mse"            
#> [10] "msle"            "npv"             "ppv"            
#> [13] "pr_auc"          "precision"       "r2"             
#> [16] "recall"          "rmse"            "rmsle"          
#> [19] "roc_auc"         "roc_index"       "sensitivity"    
#> [22] "specificity"     "weighted_kappa2"

## Metrics available for resample output
metricinfo(res) %>% names
#> [1] "accuracy"      "brier"         "cross_entropy" "kappa2"       
#> [5] "pr_auc"        "roc_auc"

## User-specified metrics
performance(res, c(accuracy, kappa2)) %>% summary
#>               Mean    Median         SD       Min Max NA
#> accuracy 0.9533333 0.9333333 0.03220306 0.9333333   1  0
#> kappa2   0.9300000 0.9000000 0.04830459 0.9000000   1  0
```

### Model Tuning

``` r
## Tune over a grid of model parameters
gbmtune <- tune(fo, data = iris, model = GBMModel,
                grid = expand.grid(n.trees = c(25, 50, 100),
                                   interaction.depth = 1:3,
                                   n.minobsinnode = c(5, 10)))

plot(gbmtune, type = "line")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r
## Fit the selected model
gbmtunefit <- fit(fo, data = iris, model = gbmtune)
varimp(gbmtunefit)
#>                Overall
#> Petal.Length 100.00000
#> Petal.Width   28.00921
#> Sepal.Width    2.30804
#> Sepal.Length   0.00000
```

### Model Comparisons

``` r
## Model comparisons
control <- CVControl(folds = 10, repeats = 5)

gbmres <- resample(fo, data = iris, model = GBMModel(n.tree = 50), control = control)
rfres <- resample(fo, data = iris, model = RandomForestModel(ntree = 50), control = control)
nnetres <- resample(fo, data = iris, model = NNetModel(size = 5), control = control)

res <- Resamples(GBM = gbmres, RF = rfres, NNet = nnetres)
summary(res)
#> , , Accuracy
#> 
#>           Mean    Median         SD Min Max NA
#> GBM  0.9466667 0.9333333 0.05387480 0.8   1  0
#> NNet 0.9373333 1.0000000 0.11773370 0.4   1  0
#> RF   0.9560000 0.9666667 0.05321415 0.8   1  0
#> 
#> , , Kappa
#> 
#>       Mean Median         SD  Min Max NA
#> GBM  0.920   0.90 0.08081220  0.7   1  0
#> NNet 0.902   1.00 0.21236280 -0.3   1  0
#> RF   0.934   0.95 0.07982123  0.7   1  0
#> 
#> , , ROCAUC
#> 
#>           Mean Median         SD       Min Max NA
#> GBM  0.9897333      1 0.01962043 0.9200000   1  0
#> NNet 0.9661333      1 0.10392602 0.3000000   1  0
#> RF   0.9939333      1 0.01405674 0.9266667   1  0
#> 
#> , , Brier
#> 
#>            Mean      Median         SD          Min       Max NA
#> GBM  0.08282749 0.063761882 0.08832055 2.740079e-05 0.3850368  0
#> NNet 0.09151662 0.005049339 0.13523087 1.367731e-51 0.6666668  0
#> RF   0.06990080 0.048453333 0.07184654 1.600000e-04 0.3176000  0

plot(res)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r
## Pairwise model differences and t-tests
perfdiff <- diff(res)
summary(perfdiff)
#> , , Accuracy
#> 
#>              Mean Median         SD         Min        Max NA
#> GBM - NNet  0.008      0 0.09955912 -0.06666667 0.53333333  0
#> GBM - RF   -0.008      0 0.03198639 -0.06666667 0.06666667  0
#> NNet - RF  -0.016      0 0.11070584 -0.60000000 0.06666667  0
#> 
#> , , Kappa
#> 
#>              Mean Median         SD  Min Max NA
#> GBM - NNet  0.014      0 0.16538144 -0.1 0.9  0
#> GBM - RF   -0.012      0 0.04797959 -0.1 0.1  0
#> NNet - RF  -0.026      0 0.18273433 -1.0 0.1  0
#> 
#> , , ROCAUC
#> 
#>               Mean Median         SD         Min        Max NA
#> GBM - NNet  0.0236      0 0.10167049 -0.06666667 0.67333333  0
#> GBM - RF   -0.0042      0 0.01243231 -0.05333333 0.01666667  0
#> NNet - RF  -0.0278      0 0.10178366 -0.68000000 0.05333333  0
#> 
#> , , Brier
#> 
#>                    Mean        Median        SD         Min       Max NA
#> GBM - NNet -0.008689127  0.0007342389 0.1032869 -0.49047781 0.1318379  0
#> GBM - RF    0.012926694  0.0037022372 0.0339893 -0.04799376 0.1196612  0
#> NNet - RF   0.021615821 -0.0011220887 0.1108912 -0.13237306 0.5181868  0

t.test(perfdiff)
#> An object of class "HTestPerformanceDiff"
#> 
#> Upper diagonal: mean differences (row - column)
#> Lower diagonal: p-values
#> P-value adjustment method: holm
#> 
#> , , Accuracy
#> 
#>            GBM      NNet     RF
#> GBM         NA 0.0080000 -0.008
#> NNet 0.6236368        NA -0.016
#> RF   0.2495969 0.6236368     NA
#> 
#> , , Kappa
#> 
#>            GBM      NNet     RF
#> GBM         NA 0.0140000 -0.012
#> NNet 0.6386268        NA -0.026
#> RF   0.2495969 0.6386268     NA
#> 
#> , , ROCAUC
#> 
#>             GBM      NNet      RF
#> GBM          NA 0.0236000 -0.0042
#> NNet 0.11847985        NA -0.0278
#> RF   0.06239545 0.1184798      NA
#> 
#> , , Brier
#> 
#>             GBM         NNet         RF
#> GBM          NA -0.008689127 0.01292669
#> NNet 0.55467315           NA 0.02161582
#> RF   0.02928367  0.348712945         NA

plot(perfdiff)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

### Ensemble Models

``` r
## Stacked regression
stackedres <- resample(fo, data = iris, model = StackedModel(GBMModel, RandomForestModel, NNetModel))
summary(stackedres)
#>                Mean     Median         SD          Min       Max NA
#> Accuracy 0.95333333 0.96666667 0.05488484 8.666667e-01 1.0000000  0
#> Kappa    0.93000000 0.95000000 0.08232726 8.000000e-01 1.0000000  0
#> ROCAUC   0.99866667 1.00000000 0.00421637 9.866667e-01 1.0000000  0
#> Brier    0.07104383 0.04671106 0.07601605 4.280279e-05 0.1982373  0

## Super learner
superres <- resample(fo, data = iris, model = SuperModel(GBMModel, RandomForestModel, NNetModel))
summary(superres)
#>                Mean    Median         SD          Min       Max NA
#> Accuracy 0.95333333 0.9666667 0.06324555 8.000000e-01 1.0000000  0
#> Kappa    0.93000000 0.9500000 0.09486833 7.000000e-01 1.0000000  0
#> ROCAUC   0.98133333 1.0000000 0.03155243 9.200000e-01 1.0000000  0
#> Brier    0.06817479 0.0322495 0.09179246 4.466662e-06 0.2782804  0
```

### Calibration Curves

``` r
cal <- calibration(res)
plot(cal, se = TRUE)
```

<img src="man/figures/README-unnamed-chunk-14-1.png" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-14-2.png" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-14-3.png" style="display: block; margin: auto;" />

### Confusion Matrices

``` r
(conf <- confusion(gbmres, cutoff = NULL))
#> GBMModel :
#>             Observed
#> Predicted          setosa   versicolor    virginica
#>   setosa     249.24113696   0.25037765   0.09683350
#>   versicolor   0.74235768 228.85744589  23.18256928
#>   virginica    0.01650536  20.89217646 226.72059721

summary(conf)
#> GBMModel :
#> Number of responses: 750
#> Accuracy (SE): 0.9397589 (0.008688084)
#> Majority class: 0.3333333
#> Kappa: 0.9096384
#> 
#>                setosa versicolor virginica
#> Observed    0.3333333  0.3333333 0.3333333
#> Predicted   0.3327845  0.3370432 0.3301724
#> Agreement   0.3323215  0.3051433 0.3022941
#> Sensitivity 0.9969645  0.9154298 0.9068824
#> Specificity 0.9993056  0.9521501 0.9581826
#> PPV         0.9986089  0.9053537 0.9155646
#> NPV         0.9984835  0.9574783 0.9536609
```

``` r
plot(conf)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

### Partial Dependence Plots

``` r
pd <- dependence(gbmfit, select = c(Petal.Length, Petal.Width))
plot(pd)
```

<img src="man/figures/README-unnamed-chunk-17-1.png" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-17-2.png" style="display: block; margin: auto;" />

### Lift Curves

``` r
## Requires a binary outcome
fo_versicolor <- factor(Species == "versicolor") ~ .
control = CVControl()

gbmres_versicolor <- resample(fo_versicolor, data = iris,  model = GBMModel, control = control)
lf <- lift(gbmres_versicolor)
plot(lf)
```

<img src="man/figures/README-unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

``` r
rfres_versicolor <- resample(fo_versicolor, data = iris,  model = RandomForestModel, control = control)
nnetres_versicolor <- resample(fo_versicolor, data = iris,  model = NNetModel, control = control)

res_versicolor <- Resamples(gbmres_versicolor, rfres_versicolor, nnetres_versicolor)
lf <- lift(res_versicolor)
plot(lf, find = 75)
```

<img src="man/figures/README-unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

### Preprocessing Recipes

``` r
library(recipes)

rec <- recipe(fo, data = iris) %>%
  add_role(Species, new_role = "case_strata") %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors())

fit_rec <- fit(rec, model = GBMModel)
varimp(fit_rec)
#>        Overall
#> PC1 100.000000
#> PC3   4.946313
#> PC2   1.089649
#> PC4   0.000000

res_rec <- resample(rec, model = GBMModel, control = CVControl)
summary(res_rec)
#>                Mean     Median         SD         Min       Max NA
#> Accuracy 0.96000000 0.96666667 0.04661373 0.866666667 1.0000000  0
#> Kappa    0.94000000 0.95000000 0.06992059 0.800000000 1.0000000  0
#> ROCAUC   0.99866667 1.00000000 0.00421637 0.986666667 1.0000000  0
#> Brier    0.06446083 0.04011043 0.05647578 0.003933401 0.1399515  0
```
