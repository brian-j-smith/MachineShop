NNetModel <- function(size, linout = NULL, entropy = NULL, softmax = NULL,
                      censored = NULL, skip = NULL, rang = NULL, decay = NULL,
                      maxit = NULL, Hess = NULL, trace = NULL, MaxNWts = NULL,
                      abstol = NULL, reltol = NULL) {
  MLModel(
    name = "NNetModel",
    packages = "nnet",
    responses = c("factor", "numeric"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      mfit <- nnet::nnet(formula, data = data, weights = weights, trace = FALSE,
                         ...)
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
