#' Flexible and Penalized Discriminant Analysis Models
#'
#' Performs flexible discriminant analysis.
#' 
#' @rdname FDAModel
#' 
#' @param theta optional matrix of class scores, typically with number of
#' columns less than one minus the number of classes.
#' @param dimension dimension of the discriminant subspace, less than the number
#' of classes, to use for prediction.
#' @param eps numeric threshold for small singular values for excluding
#' discriminant variables.
#' @param method regression function used in optimal scaling.  The default of
#' linear regression is provided by \code{\link[mda]{polyreg}} from the
#' \pkg{mda} package.  For penalized discriminant analysis,
#' \code{\link[mda]{gen.ridge}} is appropriate.  Other possibilities are
#' \code{\link[mda]{mars}} for multivariate adaptive regression splines and
#' \code{\link[mda]{bruto}} for adaptive backfitting of additive splines.  Use
#' the \code{\link[MachineShop:dot-]{.}} operator to quote specified functions.
#' @param ... additional arguments to \code{method} for \code{FDAModel} and to
#' \code{FDAModel} for \code{PDAModel}.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}}
#' }
#' 
#' The \code{\link{predict}} function for this model additionally accepts the
#' following argument.
#' \describe{
#' \item{\code{prior}}{prior class membership probabilities for prediction data
#' if different from the training set.}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source links below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[mda]{fda}}, \code{\link[mda]{predict.fda}},
#' \code{\link{fit}}, \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = FDAModel())
#'
FDAModel <- function(theta = NULL, dimension = NULL, eps = .Machine$double.eps,
                     method = .(mda::polyreg), ...) {
  
  MLModel(
    name = "FDAModel",
    label = "Flexible Discriminant Analysis",
    packages = "mda",
    types = "factor",
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      mda::fda(formula, data = data, weights = weights, ...)
    },
    predict = function(object, newdata, prior = object$prior, ...) {
      predict(object, newdata = newdata, type = "posterior", prior = prior)
    }
  )
  
}


#' @rdname FDAModel
#' 
#' @param lambda shrinkage penalty coefficient.
#' @param df alternative specification of \code{lambda} in terms of equivalent
#' degrees of freedom.
#' 
#' @examples
#' fit(Species ~ ., data = iris, model = PDAModel())
#' 
PDAModel <- function(lambda = 1, df = NULL, ...) {
  args <- c(as.list(environment()), list(...))
  args$method <- .(mda::gen.ridge)
  model <- do.call(FDAModel, args, quote = TRUE)
  model@name <- "PDAModel"
  model@label <- "Penalized Discriminant Analysis"
  model
}
