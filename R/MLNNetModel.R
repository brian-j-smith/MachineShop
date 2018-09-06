#' Neural Network Model
#' 
#' Fit single-hidden-layer neural network, possibly with skip-layer connections.
#' 
#' @param size number of units in the hidden layer.
#' @param linout switch for linear output units.
#' @param entropy switch for entropy (= maximum conditional likelihood) fitting.
#' @param softmax switch for softmax (log-linear model) and maximum conditional
#' likelihood fitting.
#' @param censored a variant on softmax, in which non-zero targets mean possible
#' classes. 
#' @param skip switch to add skip-layer connections from input to output.
#' @param rang Initial random weights on [\code{-rang}, \code{rang}].
#' @param decay parameter for weight decay.
#' @param maxit maximum number of iterations.
#' @param trace switch for tracing optimization.
#' @param MaxNWts maximum allowable number of weights.
#' @param abstol stop if the fit criterion falls below \code{abstol}, indicating
#' an essentially perfect fit.
#' @param reltol stop if the optimizer is unable to reduce the fit criterion by
#' a factor of at least \code{1 - reltol}.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @seealso \code{\link[nnet]{nnet}}
#' 
NNetModel <- function(size = 1, linout = NULL, entropy = NULL, softmax = NULL,
                      censored = NULL, skip = NULL, rang = NULL, decay = NULL,
                      maxit = NULL, trace = FALSE, MaxNWts = NULL,
                      abstol = NULL, reltol = NULL) {
  MLModel(
    name = "NNetModel",
    packages = "nnet",
    types = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      mfit <- nnet::nnet(formula, data = data, weights = weights, ...)
      mfit$y <- response(formula, data)
      asMLModelFit(mfit, "NNetFit", NNetModel(...))
    },
    predict = function(object, newdata, ...) {
      predict(asParentFit(object), newdata = newdata, type = "raw")
    },
    response = function(object, ...) {
      object$y
    },
    varimp = function(object, ...) {
      nvars <- object$n[1]
      size <- object$n[2]
      nresp <- object$n[3]
      
      beta <- abs(coef(object))
      nms <- names(beta)
      
      idx <- expand.grid(hidden = 1:size, input = 1:nvars)
      labels <- paste0("i", idx$input, "->h", idx$hidden)
      i2h <- matrix(beta[match(labels, nms)], size, nvars)
      
      idx <- expand.grid(hidden = 1:size,
                         output = if(nresp == 1) "" else 1:nresp)
      labels <- paste0("h", idx$hidden, "->o", idx$output)
      h2o <- matrix(beta[match(labels, nms)], size, nresp)
      
      vi <- sapply(1:nresp, function(output) {
        100 * ((i2h * h2o[, output]) %>%
          prop.table(margin = 1) %>%
          colSums %>%
          prop.table)
      })
      dimnames(vi) <- list(object$coefnames, colnames(object$residuals))
      drop(vi)
    }
  )
}
