#' Display Model Information
#' 
#' Display information about models provided by the \pkg{MachineShop} package.
#' 
#' @param ... \code{MLModel} objects, constructor functions, character
#' strings naming constructor functions, or supported responses for which to
#' display information.  If none are specified, information is returned on all
#' available models by default.
#' 
#' @return List of named models containing the \code{"label"}, required
#' \code{"packages"}, supported response variable \code{"types"}, the
#' constructor \code{"arguments"}, and whether a \code{"varimp"} function is
#' implemented for each.
#' 
#' @examples
#' ## All models
#' modelinfo()
#' 
#' ## Models by response types
#' names(modelinfo(factor(0)))
#' names(modelinfo(factor(0), numeric(0)))
#' 
modelinfo <- function(...) {
  args <- list(...)
  if (length(args) == 0) args <- list(NULL)
  do.call(.modelinfo, args)
}


setGeneric(".modelinfo", function(x, ...) standardGeneric(".modelinfo"))


setMethod(".modelinfo", "NULL",
  function(x, ...) {
    model_labels <- c(
      "AdaBagModel" = "Bagging with Classification Trees",
      "AdaBoostModel" = "Boosting with Classification Trees",
      "C50Model" = "C5.0 Classification",
      "CForestModel" = "Conditional Random Forests",
      "CoxModel" = "Cox Regression",
      "CoxStepAICModel" = "Cox Regression (Stepwise)",
      "EarthModel" = "Multivariate Adaptive Regression Splines",
      "FDAModel" = "Flexible Discriminant Analysis",
      "GBMModel" = "Generalized Boosted Regression",
      "GLMModel" = "Generalized Linear Models",
      "GLMStepAICModel" = "Generalized Linear Models (Stepwise)",
      "GLMNetModel" = "Lasso and Elastic-Net",
      "KNNModel" = "K-Nearest Neighbors Model",
      "LDAModel" = "Linear Discriminant Analysis",
      "LMModel" = "Linear Model",
      "MDAModel" = "Mixture Discriminant Analysis",
      "NaiveBayesModel" = "Naive Bayes Classifier",
      "NNetModel" = "Feed-Forward Neural Networks",
      "PDAModel" = "Penalized Discriminant Analysis",
      "PLSModel" = "Partial Least Squares",
      "POLRModel" = "Ordered Logistic Regression",
      "QDAModel" = "Quadratic Discriminant Analysis",
      "RandomForestModel" = "Random Forests",
      "RangerModel" = "Fast Random Forests",
      "RPartModel" = "Recursive Partitioning and Regression Trees",
      "StackedModel" = "Stacked Regression",
      "SuperModel" = "Super Learner",
      "SurvRegModel" = "Parametric Survival",
      "SurvRegStepAICModel" = "Parametric Survival (Stepwise)",
      "SVMModel" = "Support Vector Machines",
      "SVMANOVAModel" = "Support Vector Machines (ANOVA)",
      "SVMBesselModel" = "Suplport Vector Machines (Bessel)",
      "SVMLaplaceModel" = "Support Vector Machines (Laplace)",
      "SVMLinearModel" = "Support Vector Machines (Linear)",
      "SVMPolyModel" = "Support Vector Machines (Poly)",
      "SVMRadialModel" = "Support Vector Machines (Radial)",
      "SVMSplineModel" = "Support Vector Machines (Spline)",
      "SVMTanhModel" = "Support Vector Machines (Tanh)",
      "TreeModel" = "Regression and Classification Trees",
      "XGBModel" = "Extreme Gradient Boosting",
      "XGBDARTModel" = "Extreme Gradient Boosting (DART)",
      "XGBLinearModel" = "Extreme Gradient Boosting (Linear)",
      "XGBTreeModel" = "Extreme Gradient Boosting (Tree)"
    )
    
    info <- list()
    for (name in names(model_labels)) {
      model_function <- get(name, mode = "function")
      model <- model_function()
      info[[model@name]] <- list(
        label = model_labels[[model@name]],
        packages = model@packages,
        types = model@types,
        arguments = args(model_function),
        varimp = !is.null(body(fitbit(model, "varimp")))
      )
    }
    info
  }
)


setMethod(".modelinfo", "ANY",
  function(x, ...) {
    args <- list(x, ...)
    info <- modelinfo()
    is_type <- sapply(info, function(this) {
      all(sapply(args, function(object) {
        any(sapply(this$types, function(type) is_response(object, type)))
      }))
    })
    info[is_type]
  }
)


setMethod(".modelinfo", "character",
  function(x, ...) {
    modelinfo(getMLObject(x, "MLModel"), ...)
  }
)


setMethod(".modelinfo", "function",
  function(x, ...) {
    modelinfo(getMLObject(x, "MLModel"), ...)
  }
)


setMethod(".modelinfo", "MLModel",
  function(x, ...) {
    model_names <- sapply(list(x, ...), function(object) {
      getMLObject(object, "MLModel")@name
    })
    modelinfo()[unique(model_names)]
  }
)
