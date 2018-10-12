
<!-- README.md is generated from README.Rmd. Please edit that file -->
MachineShop: Machine Learning Models and Tools
==============================================

Overview
--------

`MachineShop` is a meta-package for statistical and machine learning with a common interface for model fitting, prediction, performance assessment, and presentation of results. Support is provided for predictive modeling of numerical, categorical, and censored time-to-event outcomes, including those listed in the table below, and for resample (bootstrap and cross-validation) estimation of model performance.

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="border-bottom:hidden" colspan="1">
</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4">
Response Variable Types

</th>
</tr>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:center;">
factor
</th>
<th style="text-align:center;">
numeric
</th>
<th style="text-align:center;">
ordered
</th>
<th style="text-align:center;">
Surv
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
C5.0 Classification
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Conditional Inference Trees
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
x
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox Regression
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
x
</td>
</tr>
<tr>
<td style="text-align:left;">
Generalized Linear Models
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Gradient Boosted Models
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
x
</td>
</tr>
<tr>
<td style="text-align:left;">
Lasso and Elastic-Net
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
x
</td>
</tr>
<tr>
<td style="text-align:left;">
Feed-Forward Neural Networks
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
x
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
x
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Ordered Logistic Regression
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Random Forests
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Parametric Survival Regression
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
x
</td>
</tr>
<tr>
<td style="text-align:left;">
Support Vector Machines
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
</tbody>
</table>

Installation
------------

``` r
# Development version from GitHub
# install.packages("devtools")
devtools::install_github("brian-j-smith/MachineShop")

# Development version with vignettes
devtools::install_github("brian-j-smith/MachineShop", build_vignettes = TRUE)
```

Example
-------

The following is a brief example using the package to apply gradient boosted models to predict the species of flowers in Edgar Anderson's iris data set.

### Training and Test Set Analysis

``` r
## Load the package
library(MachineShop)
library(magrittr)

## Iris flower species (3 level response) data set
df <- iris
df$Species <- factor(df$Species)

## Training and test sets
set.seed(123)
trainindices <- sample(nrow(df), nrow(df) * 2 / 3)
train <- df[trainindices, ]
test <- df[-trainindices, ]

## Model formula
fo <- Species ~ .

## Gradient boosted mode fit to training set
gbmfit <- fit(fo, data = train, model = GBMModel)

## Variable importance
(vi <- varimp(gbmfit))
#>                  Overall
#> Petal.Length 100.0000000
#> Petal.Width   12.9638575
#> Sepal.Width    0.1409401
#> Sepal.Length   0.0000000

plot(vi)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
## Test set predicted probabilities
predict(gbmfit, newdata = test, type = "prob") %>% head
#>         setosa   versicolor    virginica
#> [1,] 0.9999755 2.449128e-05 2.828117e-08
#> [2,] 0.9999365 6.346918e-05 6.535304e-09
#> [3,] 0.9999365 6.346918e-05 6.535304e-09
#> [4,] 0.9999755 2.449128e-05 2.828117e-08
#> [5,] 0.9998941 1.059313e-04 8.577135e-09
#> [6,] 0.9999291 7.084465e-05 5.736212e-09

## Test set predicted classifications
predict(gbmfit, newdata = test) %>% head
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica

## Test set performance
obs <- response(fo, data = test)
pred <- predict(gbmfit, newdata = test, type = "prob")
modelmetrics(obs, pred)
#>  Accuracy     Kappa  MLogLoss 
#> 0.9200000 0.8793727 0.4522663
```

### Resampling

``` r
## Resample estimation of model performance
(perf <- resample(fo, data = df, model = GBMModel, control = CVControl))
#> An object of class "Resamples"
#> 
#> metrics: Accuracy, Kappa, MLogLoss
#> 
#> method: 10-Fold CV
#> 
#> resamples: 10

summary(perf)
#>               Mean    Median         SD         Min       Max NA
#> Accuracy 0.9466667 0.9333333 0.05258738 0.866666667 1.0000000  0
#> Kappa    0.9200000 0.9000000 0.07888106 0.800000000 1.0000000  0
#> MLogLoss 0.2552582 0.1063595 0.28612682 0.001919676 0.7497674  0

plot(perf)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

### Model Tuning

``` r
## Model tuning
gbmtune <- tune(fo, data = df, model = GBMModel,
                grid = expand.grid(n.trees = c(25, 50, 100),
                                   interaction.depth = 1:3,
                                   n.minobsinnode = c(5, 10)))

plot(gbmtune, type = "line")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
## Fit the tuned model
gbmtunefit <- fit(fo, data = df, model = gbmtune)

varimp(gbmtunefit)
#>                Overall
#> Petal.Length 100.00000
#> Petal.Width   36.94008
#> Sepal.Length   0.00000
#> Sepal.Width    0.00000
```

### Model Comparisons

``` r
## Model comparisons
control <- CVControl(folds = 10, repeats = 5)

gbmperf <- resample(fo, data = df, model = GBMModel, control = control)
rfperf <- resample(fo, data = df, model = RandomForestModel, control = control)
nnetperf <- resample(fo, data = df, model = NNetModel, control = control)

perf <- Resamples(GBM = gbmperf, RF = rfperf, NNet = nnetperf)

summary(perf)
#> , , Accuracy
#> 
#>           Mean    Median         SD       Min Max NA
#> GBM  0.9413333 0.9333333 0.04595176 0.8666667   1  0
#> RF   0.9560000 0.9333333 0.04782811 0.8666667   1  0
#> NNet 0.8026667 0.9333333 0.22170643 0.2000000   1  0
#> 
#> , , Kappa
#> 
#>       Mean Median         SD  Min Max NA
#> GBM  0.912    0.9 0.06892765  0.8   1  0
#> RF   0.934    0.9 0.07174217  0.8   1  0
#> NNet 0.704    0.9 0.33255965 -0.2   1  0
#> 
#> , , MLogLoss
#> 
#>           Mean     Median         SD          Min       Max NA
#> GBM  0.2701410 0.20428253 0.26370125 8.629640e-04 0.9391568  0
#> RF   0.1163476 0.09404182 0.09769631 8.437327e-03 0.3860373  0
#> NNet 0.4244166 0.46209812 0.46775866 8.472304e-05 1.8545264  0

plot(perf)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
## Pairwise model differences and t-tests
perfdiff <- diff(perf)

summary(perfdiff)
#> , , Accuracy
#> 
#>                   Mean Median         SD         Min        Max NA
#> GBM - RF   -0.01466667      0 0.03636549 -0.06666667 0.06666667  0
#> GBM - NNet  0.13866667      0 0.22691463 -0.13333333 0.73333333  0
#> RF - NNet   0.15333333      0 0.22985749 -0.13333333 0.73333333  0
#> 
#> , , Kappa
#> 
#>              Mean Median         SD  Min Max NA
#> GBM - RF   -0.022      0 0.05454824 -0.1 0.1  0
#> GBM - NNet  0.208      0 0.34037195 -0.2 1.1  0
#> RF - NNet   0.230      0 0.34478624 -0.2 1.1  0
#> 
#> , , MLogLoss
#> 
#>                  Mean       Median        SD         Min       Max NA
#> GBM - RF    0.1537934  0.073026764 0.1913883 -0.03670239 0.5726908  0
#> GBM - NNet -0.1542756  0.001171424 0.4843939 -1.71381702 0.6686546  0
#> RF - NNet  -0.3080690 -0.205618326 0.4625920 -1.77270930 0.2399817  0

t.test(perfdiff)
#> An object of class "ResamplesHTest"
#> 
#> upper diagonal: mean differences (row - column)
#> lower diagonal: p-values
#> p-value adjustment: holm
#> 
#> , , Accuracy
#> 
#>               GBM            RF      NNet
#> GBM            NA -1.466667e-02 0.1386667
#> RF   0.0063465898            NA 0.1533333
#> NNet 0.0001513451  6.080819e-05        NA
#> 
#> , , Kappa
#> 
#>               GBM            RF  NNet
#> GBM            NA -2.200000e-02 0.208
#> RF   0.0063465898            NA 0.230
#> NNet 0.0001513451  6.080819e-05    NA
#> 
#> , , MLogLoss
#> 
#>               GBM           RF       NNet
#> GBM            NA 1.537934e-01 -0.1542756
#> RF   2.160468e-06           NA -0.3080690
#> NNet 2.883235e-02 4.163375e-05         NA

plot(perfdiff)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

### Recipes

``` r
library(recipes)

rec <- recipe(fo, data = df) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors())

perf <- resample(rec, model = GBMModel, control = CVControl)

summary(perf)
#>               Mean    Median         SD        Min       Max NA
#> Accuracy 0.9466667 0.9333333 0.04216370 0.86666667 1.0000000  0
#> Kappa    0.9200000 0.9000000 0.06324555 0.80000000 1.0000000  0
#> MLogLoss 0.1433132 0.1400161 0.12644266 0.01092646 0.3969964  0
```

Documentation
-------------

Once the package is installed, general documentation on its usage can be viewed with the following console commands.

``` r
library(MachineShop)

# Package help summary
?MachineShop

# Vignette
RShowDoc("Introduction", package = "MachineShop")
```
