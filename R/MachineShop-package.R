#' @details
#' MachineShop provides a common interface to machine learning and statistical
#' models provided by other packages.  Supported models are summarized in the
#' table below according to the types of response variables with which each can
#' be used.
#' 
#' \tabular{lcccc}{
#'   \strong{Model Objects} \tab \strong{factor} \tab \strong{numeric} \tab \strong{ordered} \tab \strong{Surv} \cr
#'   \code{\link{C50Model}}            \tab x \tab   \tab   \tab   \cr
#'   \code{\link{CForestModel}}        \tab x \tab x \tab   \tab x \cr
#'   \code{\link{CoxModel}}            \tab   \tab   \tab   \tab x \cr
#'   \code{\link{CoxStepAICModel}}     \tab   \tab   \tab   \tab x \cr
#'   \code{\link{GBMModel}}            \tab x \tab x \tab   \tab x \cr
#'   \code{\link{GLMModel}}            \tab x \tab x \tab   \tab   \cr
#'   \code{\link{GLMStepAICModel}}     \tab x \tab x \tab   \tab   \cr
#'   \code{\link{GLMNetModel}}         \tab x \tab x \tab   \tab x \cr
#'   \code{\link{NNetModel}}           \tab x \tab x \tab   \tab   \cr
#'   \code{\link{PLSModel}}            \tab x \tab x \tab   \tab   \cr
#'   \code{\link{POLRModel}}           \tab   \tab   \tab x \tab   \cr
#'   \code{\link{RandomForestModel}}   \tab x \tab x \tab   \tab   \cr
#'   \code{\link{StackedModel}}        \tab x \tab x \tab x \tab x \cr
#'   \code{\link{SuperModel}}          \tab x \tab x \tab x \tab x \cr
#'   \code{\link{SurvRegModel}}        \tab   \tab   \tab   \tab x \cr
#'   \code{\link{SurvRegStepAICModel}} \tab   \tab   \tab   \tab x \cr
#'   \code{\link{SVMModel}}            \tab x \tab x \tab   \tab   \cr
#' }
#' 
#' The following set of standard model fitting, prediction, performance
#' assessment, and tuning functions are available for the model objects.
#' 
#' \tabular{ll}{
#'   \code{\link{fit}} \tab Model Fitting \cr
#'   \code{\link{predict}} \tab Model Prediction \cr
#'   \code{\link{dependence}} \tab Parital Dependence \cr
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
