#' @details
#' The following set of model training, prediction, performance assessment, and
#' tuning functions are available for \pkg{MachineShop} \link{models}.
#' 
#' Training: 
#' \tabular{ll}{
#'   \code{\link{fit}} \tab Model Fitting \cr
#'   \code{\link{resample}} \tab Resample Estimation of Model Performance \cr
#'   \code{\link{tune}} \tab Model Tuning and Selection \cr
#' }
#'
#' Response Values:
#' \tabular{ll}{
#'   \code{\link{response}} \tab Observed \cr
#'   \code{\link{predict}} \tab Predicted \cr
#' }
#'
#' Performance Assessment:
#' \tabular{ll}{
#'   \code{\link{calibration}} \tab Model Calibration \cr
#'   \code{\link{confusion}} \tab Confusion Matrix \cr
#'   \code{\link{dependence}} \tab Parital Dependence \cr
#'   \code{\link{diff}} \tab Model Performance Differences \cr
#'   \code{\link{lift}} \tab Lift Curves \cr
#'   \code{\link{performance} \link{metrics}} \tab Model Performance Metrics \cr
#'   \code{\link{performance_curve}} \tab Model Performance Curves \cr
#'   \code{\link{varimp}} \tab Variable Importance \cr
#' }
#' 
#' Methods for resample estimation include
#' 
#' \tabular{ll}{
#'   \code{\link{BootControl}} \tab Simple Bootstrap \cr
#'   \code{\link{BootOptimismControl}} \tab Optimism-Corrected Bootstrap \cr
#'   \code{\link{CVControl}} \tab Repeated K-Fold Cross-Validation \cr
#'   \code{\link{OOBControl}} \tab Out-of-Bootstrap \cr
#'   \code{\link{SplitControl}} \tab Split Training-Testing \cr
#'   \code{\link{TrainControl}} \tab Training Resubstitution \cr
#' }  
#' 
#' Graphical and tabular summaries of modeling results can be obtained with
#' 
#' \code{\link{plot}} \cr
#' \code{\link{summary}}
#' 
#' Further information on package features is available with
#' 
#' \tabular{ll}{
#'   \code{\link{metricinfo}} \tab Performance Metric Information \cr
#'   \code{\link{modelinfo}} \tab Model Information \cr
#'   \code{\link{settings}} \tab Global Settings \cr
#' }
#' 
#' Custom metrics and models can be created with the \code{\link{MLMetric}} and
#' \code{\link{MLModel}} constructors.
#' 
"_PACKAGE"
