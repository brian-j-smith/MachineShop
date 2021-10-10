#' @details
#' The following set of model fitting, prediction, and performance assessment
#' functions are available for \pkg{MachineShop} \link{models}.
#'
#' Training:
#' \tabular{ll}{
#'   \code{\link{fit}} \tab Model fitting \cr
#'   \code{\link{resample}} \tab Resample estimation of model performance \cr
#' }
#'
#' Tuning Grids:
#' \tabular{ll}{
#'   \code{\link{expand_model}} \tab Model expansion over tuning parameters \cr
#'   \code{\link{expand_modelgrid}} \tab Model tuning grid expansion \cr
#'   \code{\link{expand_params}} \tab Model parameters expansion \cr
#'   \code{\link{expand_steps}} \tab Recipe step parameters expansion \cr
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
#'   \code{\link{calibration}} \tab Model calibration \cr
#'   \code{\link{confusion}} \tab Confusion matrix \cr
#'   \code{\link{dependence}} \tab Parital dependence \cr
#'   \code{\link{diff}} \tab Model performance differences \cr
#'   \code{\link{lift}} \tab Lift curves \cr
#'   \code{\link{performance} \link{metrics}} \tab Model performance metrics \cr
#'   \code{\link{performance_curve}} \tab Model performance curves \cr
#'   \code{\link{rfe}} \tab Recursive feature elimination \cr
#'   \code{\link{varimp}} \tab Variable importance \cr
#' }
#'
#' Methods for resample estimation include
#'
#' \tabular{ll}{
#'   \code{\link{BootControl}} \tab Simple bootstrap \cr
#'   \code{\link{BootOptimismControl}} \tab Optimism-corrected bootstrap \cr
#'   \code{\link{CVControl}} \tab Repeated K-fold cross-validation \cr
#'   \code{\link{CVOptimismControl}} \tab Optimism-corrected cross-validation \cr
#'   \code{\link{OOBControl}} \tab Out-of-bootstrap \cr
#'   \code{\link{SplitControl}} \tab Split training-testing \cr
#'   \code{\link{TrainControl}} \tab Training resubstitution \cr
#' }
#'
#' Graphical and tabular summaries of modeling results can be obtained with
#'
#'\tabular{l}{
#'   \code{\link{plot}} \cr
#'   \code{\link{print}} \cr
#'   \code{\link{summary}} \cr
#'}
#'
#' Further information on package features is available with
#'
#' \tabular{ll}{
#'   \code{\link{metricinfo}} \tab Performance metric information \cr
#'   \code{\link{modelinfo}} \tab Model information \cr
#'   \code{\link{settings}} \tab Global settings \cr
#' }
#'
#' Custom metrics and models can be created with the \code{\link{MLMetric}} and
#' \code{\link{MLModel}} constructors.
#'
"_PACKAGE"
