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
#' \item{\link[=tune]{Automatic Tuning} Grid Parameters:}{
#'   \code{size}, \code{decay}
#' }
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[nnet]{nnet}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#' 
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = NNetModel())
#' 
NNetModel <- function(size = 1, linout = FALSE, entropy = NULL, softmax = NULL,
                      censored = FALSE, skip = FALSE, rang = 0.7, decay = 0,
                      maxit = 100, trace = FALSE, MaxNWts = 1000, abstol = 1e-4,
                      reltol = 1e-8) {
  
  MLModel(
    name = "NNetModel",
    label = "Feed-Forward Neural Networks",
    packages = "nnet",
    response_types = c("factor", "numeric"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    grid = function(x, length, ...) {
      list(
        size = round(seq_range(1, 2, c(1, 20), length = length)),
        decay = c(0, 10^seq_inner(-5, 1, length - 1))
      )
    },
    fit = function(formula, data, weights, ...) {
      eval_fit(data,
               formula = nnet::nnet(formula, data = as.data.frame(data),
                                    weights = weights, ...),
               matrix = nnet::nnet(x, y, weights = weights, ...))
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "raw")
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
                         output = if (nresp == 1) "" else 1:nresp)
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
