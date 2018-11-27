#' Model Performance Differences
#' 
#' Pairwise model differences in resampled performance metrics.
#' 
#' @name diff
#' @rdname diff-methods
#' 
#' @param x object containing resampled metrics.
#' @param ... arguments to be passed to other methods.
#' 
#' @return \code{ModelMetricsDiff} class object that inherits from
#' \code{ModelMetrics}.
#' 
#' @seealso \code{\link{plot}}, \code{\link{modelmetrics}},
#' \code{\link{summary}}, \code{\link{t.test}}
#' 
diff.ModelMetrics <- function(x, ...) {
  if (length(dim(x)) <= 2) stop("more than one model needed to diff")
  indices <- combn(dim(x)[3], 2)
  indices1 <- indices[1,]
  indices2 <- indices[2,]
  xdiff <- x[, , indices1, drop = FALSE] - x[, , indices2, drop = FALSE]
  model_names <- dimnames(x)[[3]]
  dimnames(xdiff)[[3]] <-
    paste(model_names[indices1], "-", model_names[indices2])
  ModelMetricsDiff(xdiff, model_names = model_names)
}


#' @rdname diff-methods
#' 
#' @seealso \code{\link{resample}}, \code{\link{Resamples}}
#' 
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' fo <- Surv(time, status != 2) ~ sex + age + year + thickness + ulcer
#' control <- CVControl()
#' 
#' gbmres1 <- resample(fo, Melanoma, GBMModel(n.trees = 25), control)
#' gbmres2 <- resample(fo, Melanoma, GBMModel(n.trees = 50), control)
#' gbmres3 <- resample(fo, Melanoma, GBMModel(n.trees = 100), control)
#' 
#' res <- Resamples(GBM1 = gbmres1, GBM2 = gbmres2, GBM3 = gbmres3)
#' resdiff <- diff(res)
#' summary(resdiff)
#' plot(resdiff)
#' 
diff.Resamples <- function(x, ...) {
  diff(modelmetrics(x))
}


#' @rdname diff-methods
#' 
diff.MLModelTune <- function(x, ...) {
  diff(x@resamples)
}


#' Paired t-Tests for Model Comparisons
#' 
#' Paired t-test comparisons of resampled performance metrics from different
#' models.
#' 
#' @name t.test
#' 
#' @param x object containing paired differences between resampled metrics.
#' @param adjust p-value adjustment for multiple statistical comparisons as
#' implemented by \code{\link[stats]{p.adjust}}.
#' @param ... arguments passed to other metrics.
#' 
#' @return \code{HTestResamples} class object that inherits from \code{array}.
#' p-values and mean differences are contained in the lower and upper triangular
#' portions, respectively, of the first two dimensions.  Model pairs are
#' contined in the third dimension.
#' 
#' @seealso \code{\link{diff}}
#' 
#' @examples
#' ## Numeric response example
#' library(MASS)
#' 
#' fo <- medv ~ .
#' control <- CVControl()
#' 
#' gbmres1 <- resample(fo, Boston, GBMModel(n.trees = 25), control)
#' gbmres2 <- resample(fo, Boston, GBMModel(n.trees = 50), control)
#' gbmres3 <- resample(fo, Boston, GBMModel(n.trees = 100), control)
#' 
#' res <- Resamples(GBM1 = gbmres1, GBM2 = gbmres2, GBM3 = gbmres3)
#' resdiff <- diff(res)
#' t.test(resdiff)
#' 
t.test.ModelMetricsDiff <- function(x, adjust = "holm", ...)
{
  pvalues <- x %>%
    apply(c(3, 2), function(resample) t.test(resample)$p.value) %>%
    apply(2, p.adjust, method = adjust)
  meandiffs <- apply(x, c(3, 2), mean, na.rm = TRUE)
  
  model_names <- x@model_names
  num_models <- length(model_names)
  results <- array(NA, dim = c(num_models, num_models, dim(x)[2]),
                   dimnames = list(model_names, model_names, dimnames(x)[[2]]))
  indices <- lower.tri(results[, , 1])
  results[indices] <- meandiffs
  results <- aperm(results, perm = c(2, 1, 3))
  results[indices] <- pvalues

  HTestResamples(results, adjust = adjust)
}
