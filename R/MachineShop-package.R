#' @details
#' MachineShop provides a common interface to machine learning and statistical
#' models provided by other packages.  Supported models are summarized in the
#' table below according to the types of response variables with which each can
#' be used.
#' 
#' \tabular{lcccc}{
#'   \strong{Model Objects} \tab \strong{Categorical} \tab \strong{Continuous}
#'   \tab \strong{Survival} \cr
#'   \code{\link{C50Model}}            \tab f   \tab     \tab   \cr
#'   \code{\link{CForestModel}}        \tab f   \tab n   \tab S \cr
#'   \code{\link{CoxModel}}            \tab     \tab     \tab S \cr
#'   \code{\link{CoxStepAICModel}}     \tab     \tab     \tab S \cr
#'   \code{\link{GBMModel}}            \tab f   \tab n   \tab S \cr
#'   \code{\link{GLMModel}}            \tab b   \tab n   \tab   \cr
#'   \code{\link{GLMStepAICModel}}     \tab b   \tab n   \tab   \cr
#'   \code{\link{GLMNetModel}}         \tab f   \tab m,n \tab S \cr
#'   \code{\link{KNNModel}}            \tab f,o \tab n   \tab   \cr
#'   \code{\link{LDAModel}}            \tab f   \tab     \tab   \cr
#'   \code{\link{LMModel}}             \tab f   \tab m,n \tab   \cr
#'   \code{\link{NNetModel}}           \tab f   \tab n   \tab   \cr
#'   \code{\link{PLSModel}}            \tab f   \tab n   \tab   \cr
#'   \code{\link{POLRModel}}           \tab o   \tab     \tab   \cr
#'   \code{\link{RandomForestModel}}   \tab f   \tab n   \tab   \cr
#'   \code{\link{StackedModel}}        \tab f,o \tab m,n \tab S \cr
#'   \code{\link{SuperModel}}          \tab f,o \tab m,n \tab S \cr
#'   \code{\link{SurvRegModel}}        \tab     \tab     \tab S \cr
#'   \code{\link{SurvRegStepAICModel}} \tab     \tab     \tab S \cr
#'   \code{\link{SVMModel}}            \tab f   \tab n   \tab   \cr
#'   \code{\link{XGBModel}}            \tab f   \tab n   \tab   \cr
#' }
#' 
#' Categorical: b = binary, f = factor, o = ordered;
#' Continuous: m = matrix, n = numeric;
#' Survival: S = Surv
#' 
#' The following set of standard model fitting, prediction, performance
#' assessment, and tuning functions are available for the model objects.
#' 
#' \tabular{ll}{
#'   \code{\link{fit}} \tab Model Fitting \cr
#'   \code{\link{predict}} \tab Model Prediction \cr
#'   \code{\link{calibration}} \tab Model Calibration \cr
#'   \code{\link{dependence}} \tab Parital Dependence \cr
#'   \code{\link{lift}} \tab Lift Curves \cr
#'   \code{\link{resample}} \tab Resample Estimation of Model Performance \cr
#'   \code{\link{tune}} \tab Model Tuning \cr
#'   \code{\link{diff}} \tab Model Performance Differences \cr
#'   \code{\link{varimp}} \tab Variable Importance \cr
#' }
#' 
#' Methods for resample estimation include
#' 
#' \tabular{ll}{
#'   \code{\link{BootControl}} \tab Simple Bootstrap \cr
#'   \code{\link{CVControl}} \tab Repeated K-Fold Cross-Validation \cr
#'   \code{\link{OOBControl}} \tab Out-of-Bootstrap \cr
#'   \code{\link{SplitControl}} \tab Split Training-Testing \cr
#'   \code{\link{TrainControl}} \tab Training Resubstitution \cr
#' }  
#' 
#' Tabular and graphical summaries of modeling results can be obtained with
#' 
#' \code{\link{summary}} \cr 
#' \code{\link{plot}}
#' 
"_PACKAGE"
