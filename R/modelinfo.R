#' Display Model Information
#' 
#' Display information about models provided by the \pkg{MachineShop} package.
#' 
#' @param ... \code{MLModel} objects, constructor functions, or character
#' strings naming constructor functions for which to display information.  By
#' default, information is returned on all available models.
#' 
#' @return List of named models containing the \code{"label"}, required
#' \code{"packages"}, supported response variable \code{"types"}, the
#' constructor \code{"arguments"}, and whether a \code{"varimp"} function is
#' implemented for each.
#' 
#' @examples
#' modelinfo()
#' 
modelinfo <- function(...) {
  model_labels <- c(
    "C50Model" = "C5.0 Classification",
    "CForestModel" = "Conditional Inference Trees",
    "CoxModel" = "Cox Regression",
    "CoxStepAICModel" = "Cox Regression (Stepwise)",
    "GBMModel" = "Gradient Boosted Models",
    "GLMModel" = "Generalized Linear Models",
    "GLMStepAICModel" = "Generalized Linear Models (Stepwise)",
    "GLMNetModel" = "Lasso and Elastic-Net",
    "KNNModel" = "K-Nearest Neighbors Model",
    "LDAModel" = "Linear Discriminant Analysis",
    "LMModel" = "Linear Model",
    "MDAModel" = "Mixture Discriminant Analysis",
    "NNetModel" = "Feed-Forward Neural Networks",
    "PLSModel" = "Partial Least Squares",
    "POLRModel" = "Ordered Logistic Regression",
    "QDAModel" = "Quadratic Discriminant Analysis",
    "RandomForestModel" = "Random Forests",
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
    "XGBModel" = "Extreme Gradient Boosting",
    "XGBDARTModel" = "Extreme Gradient Boosting (DART)",
    "XGBLinearModel" = "Extreme Gradient Boosting (Linear)",
    "XGBTreeModel" = "Extreme Gradient Boosting (Tree)"
  )
  
  models <- list(...)
  if (length(models) == 0) models <- names(model_labels)
  
  info <- list()
  for (model in models) {
    model <- getMLObject(model, "MLModel")
    info[[model@name]] <- list(
      label = model_labels[[model@name]],
      packages = model@packages,
      types = model@types,
      arguments = args(model@name),
      varimp = !is.null(body(fitbit(model, "varimp")))
    )
  }
  info
}
