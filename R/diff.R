#' Model Performance Differences
#' 
#' Pairwise model differences in resampled performance metrics.
#' 
#' @name diff
#' @rdname diff-methods
#' 
#' @param x model \link{tune}, \link{performance}, or \link{resample} result. 
#' @param ... arguments passed to other methods.
#' 
#' @return \code{PerformanceDiff} class object that inherits from
#' \code{Performance}.
#' 
#' @seealso \code{\link{t.test}}, \code{\link{plot}}, \code{\link{summary}}
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
#' perfdiff <- diff(res)
#' summary(perfdiff)
#' plot(perfdiff)
#' 
NULL


#' @rdname diff-methods
#' 
diff.MLModelTune <- function(x, ...) {
  diff(x@performance)
}


#' @rdname diff-methods
#' 
diff.Performance <- function(x, ...) {
  if (length(dim(x)) <= 2) stop("more than one model needed to diff")
  indices <- combn(dim(x)[3], 2)
  indices1 <- indices[1,]
  indices2 <- indices[2,]
  xdiff <- x[, , indices1, drop = FALSE] - x[, , indices2, drop = FALSE]
  model_names <- dimnames(x)[[3]]
  dimnames(xdiff)[[3]] <-
    paste(model_names[indices1], "-", model_names[indices2])
  PerformanceDiff(xdiff, model_names = model_names)
}


#' @rdname diff-methods
#' 
diff.Resamples <- function(x, ...) {
  diff(performance(x))
}


PerformanceDiff <- function(object, model_names) {
  new("PerformanceDiff", object, model_names = model_names)
}


#' Paired t-Tests for Model Comparisons
#' 
#' Paired t-test comparisons of resampled performance metrics from different
#' models.
#' 
#' @name t.test
#' 
#' @method t.test PerformanceDiff
#' 
#' @param x performance \link[=diff]{difference} result.
#' @param adjust p-value adjustment for multiple statistical comparisons as
#' implemented by \code{\link[stats]{p.adjust}}.
#' @param ... arguments passed to other methods.
#' 
#' @return \code{PerformanceDiffTest} class object that inherits from
#' \code{array}.  p-values and mean differences are contained in the lower and
#' upper triangular portions, respectively, of the first two dimensions.  Model
#' pairs are contined in the third dimension.
#' 
#' @examples
#' ## Numeric response example
#' fo <- sale_amount ~ .
#' control <- CVControl()
#' 
#' gbmres1 <- resample(fo, ICHomes, GBMModel(n.trees = 25), control)
#' gbmres2 <- resample(fo, ICHomes, GBMModel(n.trees = 50), control)
#' gbmres3 <- resample(fo, ICHomes, GBMModel(n.trees = 100), control)
#' 
#' res <- Resamples(GBM1 = gbmres1, GBM2 = gbmres2, GBM3 = gbmres3)
#' perfdiff <- diff(res)
#' t.test(perfdiff)
#' 
t.test.PerformanceDiff <- function(x, adjust = "holm", ...)
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

  PerformanceDiffTest(results, adjust = adjust)
}
