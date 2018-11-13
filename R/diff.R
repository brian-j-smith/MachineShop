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
#' @return ResamplesDiff class object that inherits from Resamples.
#' 
#' @seealso \code{\link{plot}}, \code{\link{resample}}, \code{\link{Resamples}},
#' \code{\link{summary}}, \code{\link{t.test}}
#' 
#' @examples
#' ## Survival response example
#' library(survival)
#' library(MASS)
#' 
#' fo <- Surv(time, status != 2) ~ sex + age + year + thickness + ulcer
#' control <- CVControl()
#' 
#' gbmperf1 <- resample(fo, Melanoma, GBMModel(n.trees = 25), control)
#' gbmperf2 <- resample(fo, Melanoma, GBMModel(n.trees = 50), control)
#' gbmperf3 <- resample(fo, Melanoma, GBMModel(n.trees = 100), control)
#' 
#' perf <- Resamples(GBM1 = gbmperf1, GBM2 = gbmperf2, GBM3 = gbmperf3)
#' perfdiff <- diff(perf)
#' summary(perfdiff)
#' plot(perfdiff)
#' 
diff.MLModelTune <- function(x, ...) {
  diff(x@resamples)
}


#' @rdname diff-methods
#' 
diff.Resamples <- function(x, ...) {
  if (length(dim(x)) <= 2) stop("more than one model needed to diff")
  indices <- combn(dim(x)[3], 2)
  indices1 <- indices[1,]
  indices2 <- indices[2,]
  xdiff <- x[, , indices1, drop = FALSE] - x[, , indices2, drop = FALSE]
  modelnames <- dimnames(x)[[3]]
  dimnames(xdiff)[[3]] <- paste(modelnames[indices1], "-", modelnames[indices2])
  ResamplesDiff(xdiff, control = x@control, modelnames = modelnames)
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
#' @return ResamplesHTest class object that inherits from array.  p-values and
#' mean differences are contained in the lower and upper triangular portions,
#' respectively, of the first two dimensions.  Model pairs are contined in the
#' third dimension.
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
#' gbmperf1 <- resample(fo, Boston, GBMModel(n.trees = 25), control)
#' gbmperf2 <- resample(fo, Boston, GBMModel(n.trees = 50), control)
#' gbmperf3 <- resample(fo, Boston, GBMModel(n.trees = 100), control)
#' 
#' perf <- Resamples(GBM1 = gbmperf1, GBM2 = gbmperf2, GBM3 = gbmperf3)
#' perfdiff <- diff(perf)
#' t.test(perfdiff)
#' 
t.test.ResamplesDiff <- function(x, adjust = "holm", ...)
{
  pvalues <- x %>%
    apply(c(3, 2), function(resample) t.test(resample)$p.value) %>%
    apply(2, p.adjust, method = adjust)
  meandiffs <- apply(x, c(3, 2), mean, na.rm = TRUE)
  
  nmodels <- length(x@modelnames)
  results <- array(NA, dim = c(nmodels, nmodels, dim(x)[2]),
                   dimnames = list(x@modelnames, x@modelnames, dimnames(x)[[2]]))
  indices <- lower.tri(results[, , 1])
  results[indices] <- meandiffs
  results <- aperm(results, perm = c(2, 1, 3))
  results[indices] <- pvalues

  ResamplesHTest(results, adjust = adjust)
}
