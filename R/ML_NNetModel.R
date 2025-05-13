#' Neural Network Model
#'
#' Fit single-hidden-layer neural network, possibly with skip-layer connections.
#'
#' @param size number of units in the hidden layer.
#' @param linout switch for linear output units.  Set automatically according to
#'   the class type of the response variable [numeric: \code{TRUE}, other:
#'   \code{FALSE}].
#' @param entropy switch for entropy (= maximum conditional likelihood) fitting.
#' @param softmax switch for softmax (log-linear model) and maximum conditional
#'   likelihood fitting.
#' @param censored a variant on softmax, in which non-zero targets mean possible
#'   classes.
#' @param skip switch to add skip-layer connections from input to output.
#' @param rang Initial random weights on [\code{-rang}, \code{rang}].
#' @param decay parameter for weight decay.
#' @param maxit maximum number of iterations.
#' @param trace switch for tracing optimization.
#' @param MaxNWts maximum allowable number of weights.
#' @param abstol stop if the fit criterion falls below \code{abstol}, indicating
#'   an essentially perfect fit.
#' @param reltol stop if the optimizer is unable to reduce the fit criterion by
#'   a factor of at least \code{1 - reltol}.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}, \code{numeric}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{size}, \code{decay}
#'   }
#' }
#'
#' Default argument values and further model details can be found in the source
#' See Also link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[nnet]{nnet}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = NNetModel)
#'
NNetModel <- function(
  size = 1, linout = logical(), entropy = logical(), softmax = logical(),
  censored = FALSE, skip = FALSE, rang = 0.7, decay = 0, maxit = 100,
  trace = FALSE, MaxNWts = 1000, abstol = 1e-4, reltol = 1e-8
) {

  MLModel(

    name = "NNetModel",
    label = "Feed-Forward Neural Networks",
    packages = "nnet",
    response_types = c("factor", "numeric"),
    weights = TRUE,
    predictor_encoding = "model.matrix",
    na.rm = TRUE,
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("size", "decay"),
      get_values = c(
        function(n, ...) round_int(seq_range(1, 2, c(1, 20), n)),
        function(n, ...) c(0, 10^seq_inner(-5, 1, n - 1))
      )
    ),

    fit = function(formula, data, weights, linout = NULL, ...) {
      y <- response(data)
      if (is.null(linout)) linout <- is_response(y, "numeric")
      if (is(terms(data), "ModelDesignTerms")) {
        x <- model.matrix(data, intercept = FALSE)
        if (is_response(y, "binary")) {
          y <- as.numeric(y) - 1
        } else if (is_response(y, "factor")) {
          y <- structure(
            model.matrix(~ y - 1),
            dimnames = list(names(y), levels(y))
          )
        }
        res <- nnet::nnet(x, y, weights = weights, linout = linout, ...)
        res$terms <- terms(data)
        res$coefnames <- colnames(x)
        res$xlevels <- list()
        res
      } else {
        nnet::nnet(
          formula, data = as.data.frame(formula, data = data),
          weights = weights, na.action = na.pass, linout = linout, ...
        )
      }
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "raw")
    },

    varimp = function(object, ...) {
      ninputs <- object$n[1]
      size <- object$n[2]
      noutputs <- object$n[3]

      beta <- abs(coef(object))

      inds <- expand.grid(input = seq_len(ninputs), hidden = seq_len(size))
      labels <- paste0("i", inds$input, "->h", inds$hidden)
      i2h <- matrix(beta[match(labels, names(beta))], ninputs, size)
      rownames(i2h) <- object$coefnames

      inds <- expand.grid(
        hidden = seq_len(size),
        output = if (noutputs == 1) "" else seq_len(noutputs)
      )
      labels <- paste0("h", inds$hidden, "->o", inds$output)
      h2o <- matrix(beta[match(labels, names(beta))], size, noutputs)
      colnames(h2o) <- colnames(object$residuals)

      structure(drop(i2h %*% h2o), metric = "coefficient")
    }

  )

}

MLModelFunction(NNetModel) <- NULL
