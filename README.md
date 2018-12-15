
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
#> Petal.Width   22.744996
#> Sepal.Width    2.665129
#> Sepal.Length   0.000000

plot(vi)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
## Test set predicted probabilities
predict(gbmfit, newdata = test, type = "prob") %>% head
#>         setosa   versicolor    virginica
#> [1,] 0.9999799 2.003614e-05 4.405618e-08
#> [2,] 0.9998815 1.185245e-04 3.509024e-09
#> [3,] 0.9998815 1.185245e-04 3.509024e-09
#> [4,] 0.9999799 2.003614e-05 4.405618e-08
#> [5,] 0.9998876 1.124205e-04 3.106606e-09
#> [6,] 0.9998876 1.124205e-04 3.106606e-09

## Test set predicted classifications
predict(gbmfit, newdata = test) %>% head
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica

## Test set performance
obs <- response(fo, data = test)
pred <- predict(gbmfit, newdata = test, type = "prob")
performance(obs, pred)
#>  Accuracy     Kappa     Brier 
#> 0.9200000 0.8793727 0.1586517
```

### Resampling

``` r
## Resample estimation of model performance
(res <- resample(fo, data = iris, model = GBMModel, control = CVControl))
#> An object of class "Resamples"
#> 
#> Models: GBMModel
#> 
#> Stratification variable: (strata) 
#> 
#> Resamples control object of class "CVMLControl"
#> 
#> Method: K-Fold Cross-Validation
#> 
#> Folds: 10
#> 
#> Repeats: 1
#> 
#> Seed: 1193771216

summary(res)
#>               Mean    Median         SD          Min       Max NA
#> Accuracy 0.9333333 0.9333333 0.04444444 8.666667e-01 1.0000000  0
#> Kappa    0.9000000 0.9000000 0.06666667 8.000000e-01 1.0000000  0
#> Brier    0.1133476 0.1225683 0.08788324 3.102957e-05 0.2610661  0

plot(res)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

### Performance Metrics

``` r
## Default performance metrics
performance(res) %>% summary
#>               Mean    Median         SD          Min       Max NA
#> Accuracy 0.9333333 0.9333333 0.04444444 8.666667e-01 1.0000000  0
#> Kappa    0.9000000 0.9000000 0.06666667 8.000000e-01 1.0000000  0
#> Brier    0.1133476 0.1225683 0.08788324 3.102957e-05 0.2610661  0

## All available metric functions
metricinfo() %>% names
#>  [1] "accuracy"        "brier"           "cindex"         
#>  [4] "cross_entropy"   "f_score"         "kappa2"         
#>  [7] "mae"             "mse"             "npv"            
#> [10] "ppv"             "pr_auc"          "precision"      
#> [13] "r2"              "recall"          "roc_auc"        
#> [16] "roc_index"       "sensitivity"     "specificity"    
#> [19] "weighted_kappa2"

## Metrics available for resample output
metricinfo(res) %>% names
#> [1] "accuracy"      "brier"         "cross_entropy" "kappa2"

## User-specified metrics
performance(res, c("accuracy", "kappa2")) %>% summary
#>               Mean    Median         SD       Min Max NA
#> accuracy 0.9333333 0.9333333 0.04444444 0.8666667   1  0
#> kappa2   0.9000000 0.9000000 0.06666667 0.8000000   1  0
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
#> Petal.Width   72.05566
#> Sepal.Width    6.38841
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
#>           Mean    Median         SD       Min Max NA
#> GBM  0.9440000 0.9333333 0.04539572 0.8666667   1  0
#> NNet 0.9440000 1.0000000 0.10289241 0.5333333   1  0
#> RF   0.9546667 0.9333333 0.04750462 0.8000000   1  0
#> 
#> , , Kappa
#> 
#>       Mean Median         SD Min Max NA
#> GBM  0.916    0.9 0.06809357 0.8   1  0
#> NNet 0.922    1.0 0.13893178 0.5   1  0
#> RF   0.932    0.9 0.07125694 0.7   1  0
#> 
#> , , Brier
#> 
#>            Mean     Median         SD          Min       Max NA
#> GBM  0.08306335 0.08243247 0.06980143 3.488940e-05 0.2496592  0
#> NNet 0.08128257 0.02515249 0.10807043 1.123105e-18 0.3333334  0
#> RF   0.06720533 0.06360000 0.05767464 1.066667e-04 0.2174933  0

plot(res)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r
## Pairwise model differences and t-tests
perfdiff <- diff(res)
summary(perfdiff)
#> , , Accuracy
#> 
#>                    Mean Median         SD         Min       Max NA
#> GBM - NNet  0.001333333      0 0.10388377 -0.13333333 0.3333333  0
#> GBM - RF   -0.010666667      0 0.03653967 -0.06666667 0.1333333  0
#> NNet - RF  -0.012000000      0 0.10140957 -0.33333333 0.1333333  0
#> 
#> , , Kappa
#> 
#>                     Mean Median         SD  Min Max NA
#> GBM - NNet  1.776262e-17      0 0.15386185 -0.2 0.5  0
#> GBM - RF   -1.400000e-02      0 0.05717856 -0.1 0.2  0
#> NNet - RF  -1.400000e-02      0 0.15119929 -0.5 0.2  0
#> 
#> , , Brier
#> 
#>                   Mean        Median         SD         Min       Max NA
#> GBM - NNet 0.001780776  0.0006957636 0.10587753 -0.33315703 0.1456281  0
#> GBM - RF   0.015858014  0.0128550442 0.02847402 -0.03609018 0.1002739  0
#> NNet - RF  0.014077238 -0.0026182722 0.10379223 -0.12043161 0.3307733  0

t.test(perfdiff)
#> An object of class "HTestPerformanceDiff"
#> 
#> Upper diagonal: mean differences (row - column)
#> Lower diagonal: p-values
#> P-value adjustment method: holm
#> 
#> , , Accuracy
#> 
#>            GBM        NNet          RF
#> GBM         NA 0.001333333 -0.01066667
#> NNet 0.9280563          NA -0.01200000
#> RF   0.1329455 0.813608991          NA
#> 
#> , , Kappa
#> 
#>            GBM         NNet     RF
#> GBM         NA 1.776262e-17 -0.014
#> NNet 1.0000000           NA -0.014
#> RF   0.2690569 1.000000e+00     NA
#> 
#> , , Brier
#> 
#>               GBM        NNet         RF
#> GBM            NA 0.001780776 0.01585801
#> NNet 0.9058172484          NA 0.01407724
#> RF   0.0007793155 0.684492157         NA

plot(perfdiff)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

### Ensemble Models

``` r
## Stacked regression
stackedres <- resample(fo, data = iris, model = StackedModel(GBMModel, RandomForestModel, NNetModel))
summary(stackedres)
#>               Mean    Median         SD          Min       Max NA
#> Accuracy 0.9600000 0.9333333 0.03442652 0.9333333333 1.0000000  0
#> Kappa    0.9400000 0.9000000 0.05163978 0.9000000000 1.0000000  0
#> Brier    0.0709116 0.0678664 0.05270345 0.0004191112 0.1602469  0

## Super learner
superres <- resample(fo, data = iris, model = SuperModel(GBMModel, RandomForestModel, NNetModel))
summary(superres)
#>               Mean    Median         SD          Min       Max NA
#> Accuracy 0.9400000 0.9333333 0.04919099 8.666667e-01 1.0000000  0
#> Kappa    0.9100000 0.9000000 0.07378648 8.000000e-01 1.0000000  0
#> Brier    0.1030155 0.1267535 0.07658820 5.463286e-06 0.2314161  0
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
#>   setosa     249.27272175   0.26314212   0.08897951
#>   versicolor   0.71288823 228.76094095  23.25960129
#>   virginica    0.01439002  20.97591693 226.65141920

summary(conf)
#> GBMModel :
#> Number of responses: 750
#> Accuracy (SE): 0.9395801 (0.00870014)
#> Majority class: 0.3333333
#> Kappa: 0.9093702
#> 
#>                setosa versicolor virginica
#> Observed    0.3333333  0.3333333 0.3333333
#> Predicted   0.3328331  0.3369779 0.3301890
#> Agreement   0.3323636  0.3050146 0.3022019
#> Sensitivity 0.9970909  0.9150438 0.9066057
#> Specificity 0.9992958  0.9520550 0.9580194
#> PPV         0.9985894  0.9051471 0.9152392
#> NPV         0.9985465  0.9572884 0.9535221
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
#> PC3   5.763949
#> PC2   3.360628
#> PC4   0.000000

res_rec <- resample(rec, model = GBMModel, control = CVControl)
summary(res_rec)
#>               Mean     Median         SD         Min       Max NA
#> Accuracy 0.9600000 1.00000000 0.06440612 0.800000000 1.0000000  0
#> Kappa    0.9400000 1.00000000 0.09660918 0.700000000 1.0000000  0
#> Brier    0.0740501 0.04393422 0.07794139 0.001759731 0.2409151  0
```
