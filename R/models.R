#' Models
#'
#' Model constructor functions supplied by \pkg{MachineShop} are summarized in
#' the table below according to the types of response variables with which each
#' can be used.
#' \tabular{lccc}{
#'   \strong{Function} \tab \strong{Categorical} \tab \strong{Continuous}
#'   \tab \strong{Survival} \cr
#'   \code{\link{AdaBagModel}}         \tab f   \tab     \tab   \cr
#'   \code{\link{AdaBoostModel}}       \tab f   \tab     \tab   \cr
#'   \code{\link{BARTModel}}           \tab f   \tab n   \tab S \cr
#'   \code{\link{BARTMachineModel}}    \tab b   \tab n   \tab   \cr
#'   \code{\link{BlackBoostModel}}     \tab b   \tab n   \tab S \cr
#'   \code{\link{C50Model}}            \tab f   \tab     \tab   \cr
#'   \code{\link{CForestModel}}        \tab f   \tab n   \tab S \cr
#'   \code{\link{CoxModel}}            \tab     \tab     \tab S \cr
#'   \code{\link{CoxStepAICModel}}     \tab     \tab     \tab S \cr
#'   \code{\link{EarthModel}}          \tab f   \tab n   \tab   \cr
#'   \code{\link{FDAModel}}            \tab f   \tab     \tab   \cr
#'   \code{\link{GAMBoostModel}}       \tab b   \tab n   \tab S \cr
#'   \code{\link{GBMModel}}            \tab f   \tab n   \tab S \cr
#'   \code{\link{GLMBoostModel}}       \tab b   \tab n   \tab S \cr
#'   \code{\link{GLMModel}}            \tab f   \tab m,n \tab   \cr
#'   \code{\link{GLMStepAICModel}}     \tab b   \tab n   \tab   \cr
#'   \code{\link{GLMNetModel}}         \tab f   \tab m,n \tab S \cr
#'   \code{\link{KNNModel}}            \tab f,o \tab n   \tab   \cr
#'   \code{\link{LARSModel}}           \tab     \tab n   \tab   \cr
#'   \code{\link{LDAModel}}            \tab f   \tab     \tab   \cr
#'   \code{\link{LMModel}}             \tab f   \tab m,n \tab   \cr
#'   \code{\link{MDAModel}}            \tab f   \tab     \tab   \cr
#'   \code{\link{NaiveBayesModel}}     \tab f   \tab     \tab   \cr
#'   \code{\link{NNetModel}}           \tab f   \tab n   \tab   \cr
#'   \code{\link{PDAModel}}            \tab f   \tab     \tab   \cr
#'   \code{\link{PLSModel}}            \tab f   \tab n   \tab   \cr
#'   \code{\link{POLRModel}}           \tab o   \tab     \tab   \cr
#'   \code{\link{QDAModel}}            \tab f   \tab     \tab   \cr
#'   \code{\link{RandomForestModel}}   \tab f   \tab n   \tab   \cr
#'   \code{\link{RangerModel}}         \tab f   \tab n   \tab S \cr
#'   \code{\link{RPartModel}}          \tab f   \tab n   \tab S \cr
#'   \code{\link{SurvRegModel}}        \tab     \tab     \tab S \cr
#'   \code{\link{SurvRegStepAICModel}} \tab     \tab     \tab S \cr
#'   \code{\link{SVMModel}}            \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMANOVAModel}}       \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMBesselModel}}      \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMLaplaceModel}}     \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMLinearModel}}      \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMPolyModel}}        \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMRadialModel}}      \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMSplineModel}}      \tab f   \tab n   \tab   \cr
#'   \code{\link{SVMTanhModel}}        \tab f   \tab n   \tab   \cr
#'   \code{\link{TreeModel}}           \tab f   \tab n   \tab   \cr
#'   \code{\link{XGBModel}}            \tab f   \tab n   \tab S \cr
#'   \code{\link{XGBDARTModel}}        \tab f   \tab n   \tab S \cr
#'   \code{\link{XGBLinearModel}}      \tab f   \tab n   \tab S \cr
#'   \code{\link{XGBTreeModel}}        \tab f   \tab n   \tab S \cr
#' }
#' Categorical: b = binary, f = factor, o = ordered\cr
#' Continuous: m = matrix, n = numeric\cr
#' Survival: S = Surv\cr
#' \cr
#' Models may be combined, tuned, or selected with the following meta-model
#' functions.
#' \tabular{ll}{
#'   \code{\link{StackedModel}}  \tab Stacked regression \cr
#'   \code{\link{SuperModel}}    \tab Super learner \cr
#'   \code{\link{SelectedModel}} \tab Model selection from a candidate set \cr
#'   \code{\link{TunedModel}}    \tab Model tuning over a parameter grid \cr
#' }
#'
#' @name models
#' @aliases MLModelFunction
#'
#' @seealso \code{\link{modelinfo}}, \code{\link{fit}}, \code{\link{resample}}
#'
NULL


#' Model Inputs
#'
#' Model inputs are the predictor and response variables whose relationship is
#' determined by a model fit.  Input specifications supported by
#' \pkg{MachineShop} are summarized in the table below.
#' \tabular{ll}{
#'   \code{\link{formula}}         \tab Traditional model formula \cr
#'   \code{\link{matrix}}          \tab Design matrix of predictors \cr
#'   \code{\link{ModelFrame}}      \tab Model frame \cr
#'   \code{\link[recipes]{recipe}} \tab Preprocessing recipe roles and steps \cr
#' }
#' Response variable types in the input specifications are defined by the user
#' with the functions and recipe roles:
#' \tabular{ll}{
#'   \strong{Response Functions}
#'     \tab \code{\link{BinomialVariate}} \cr
#'     \tab \code{\link{DiscreteVariate}} \cr
#'     \tab \code{\link{factor}} \cr
#'     \tab \code{\link{matrix}} \cr
#'     \tab \code{\link{NegBinomialVariate}} \cr
#'     \tab \code{\link{numeric}} \cr
#'     \tab \code{\link{ordered}} \cr
#'     \tab \code{\link{PoissonVariate}} \cr
#'     \tab \code{\link[survival]{Surv}} \cr
#'   \strong{Recipe Roles}
#'     \tab \code{\link{role_binom}} \cr
#'     \tab \code{\link{role_surv}} \cr
#' }
#' Inputs may be combined, selected, or tuned with the following meta-input
#' functions.
#' \tabular{ll}{
#'   \code{\link{ModeledInput}}  \tab Input with a prespecified model \cr
#'   \code{\link{SelectedInput}} \tab Input selection from a candidate set \cr
#'   \code{\link{TunedInput}}    \tab Input tuning over a parameter grid \cr
#' }
#'
#' @name inputs
#'
#' @seealso \code{\link{fit}}, \code{\link{resample}}
#'
NULL
