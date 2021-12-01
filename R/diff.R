#' Model Performance Differences
#'
#' Pairwise model differences in resampled performance metrics.
#'
#' @name diff
#' @rdname diff-methods
#'
#' @param x model \link{performance} or \link{resample} result.
#' @param ... arguments passed to other methods.
#'
#' @return \code{PerformanceDiff} class object that inherits from
#' \code{Performance}.
#'
#' @seealso \code{\link{t.test}}, \code{\link{plot}}, \code{\link{summary}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Survival response example
#' library(survival)
#'
#' fo <- Surv(time, status) ~ .
#' control <- CVControl()
#'
#' gbm_res1 <- resample(fo, data = veteran, GBMModel(n.trees = 25), control)
#' gbm_res2 <- resample(fo, data = veteran, GBMModel(n.trees = 50), control)
#' gbm_res3 <- resample(fo, data = veteran, GBMModel(n.trees = 100), control)
#'
#' res <- c(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
#' res_diff <- diff(res)
#' summary(res_diff)
#' plot(res_diff)
#' }
#'
NULL


#' @rdname diff-methods
#'
diff.MLModel <- function(x, ...) {
  if (!is_trained(x)) throw(Error("No training results to diff."))
  map(function(step) diff(step@performance), x@steps)
}


#' @rdname diff-methods
#'
diff.Performance <- function(x, ...) {
  if (ndim(x) <= 2) throw(Error("More than one model needed to diff."))
  indices <- combn(size(x, 3), 2)
  indices1 <- indices[1, ]
  indices2 <- indices[2, ]
  xdiff <- x[, , indices1, drop = FALSE] - x[, , indices2, drop = FALSE]
  model_names <- dimnames(x)[[3]]
  dimnames(xdiff)[[3]] <-
    paste(model_names[indices1], "-", model_names[indices2])
  PerformanceDiff(xdiff, model_names = model_names, control = x@control)
}


#' @rdname diff-methods
#'
diff.Resample <- function(x, ...) {
  diff(performance(x))
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
#'   implemented by \code{\link[stats]{p.adjust}}.
#' @param ... arguments passed to other methods.
#'
#' @details
#' The t-test statistic for pairwise model differences of \eqn{R} resampled
#' performance metric values is calculated as
#' \deqn{
#'   t = \frac{\bar{x}_R}{\sqrt{F s^2_R / R}},
#' }
#' where \eqn{\bar{x}_R} and \eqn{s^2_R} are the sample mean and variance.
#' Statistical testing for a mean difference is then performed by comparing
#' \eqn{t} to a \eqn{t_{R-1}} null distribution.  The sample variance in the
#' t statistic is known to underestimate the true variances of cross-validation
#' mean estimators.  Underestimation of these variances will lead to increased
#' probabilities of false-positive statistical conclusions.  Thus, an additional
#' factor \eqn{F} is included in the t statistic to allow for variance
#' corrections.  A correction of \eqn{F = 1 + K / (K - 1)} was found by
#' Nadeau and Bengio (2003) to be a good choice for cross-validation with
#' \eqn{K} folds and is thus used for that resampling method.  The extension of
#' this correction by Bouchaert and Frank (2004) to \eqn{F = 1 + T K / (K - 1)}
#' is used for cross-validation with \eqn{K} folds repeated \eqn{T} times.  For
#' other resampling methods \eqn{F = 1}.
#'
#' @return \code{PerformanceDiffTest} class object that inherits from
#' \code{array}.  p-values and mean differences are contained in the lower and
#' upper triangular portions, respectively, of the first two dimensions.  Model
#' pairs are contained in the third dimension.
#'
#' @references
#' Nadeau, C., & Bengio, Y. (2003). Inference for the generalization error.
#' \emph{Machine Learning}, \emph{52}, 239–81.
#'
#' Bouckaert, R. R., & Frank, E. (2004). Evaluating the replicability of
#' significance tests for comparing learning algorithms. In H. Dai, R. Srikant,
#' & C. Zhang (Eds.), \emph{Advances in knowledge discovery and data mining}
#' (pp. 3–12). Springer.
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' ## Numeric response example
#' fo <- sale_amount ~ .
#' control <- CVControl()
#'
#' gbm_res1 <- resample(fo, ICHomes, GBMModel(n.trees = 25), control)
#' gbm_res2 <- resample(fo, ICHomes, GBMModel(n.trees = 50), control)
#' gbm_res3 <- resample(fo, ICHomes, GBMModel(n.trees = 100), control)
#'
#' res <- c(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
#' res_diff <- diff(res)
#' t.test(res_diff)
#' }
#'
t.test.PerformanceDiff <- function(x, adjust = "holm", ...)
{
  vadj <- if (is(x@control, "CVControl")) 1 / (x@control@folds - 1) else 0

  t_test <- function(x) {
    x <- na.omit(x)
    n <- length(x)
    stat <- mean(x) / sqrt(var(x) * (1 / n + vadj))
    2 * pt(abs(stat), n - 1, lower.tail = FALSE)
  }

  pvalues <- x %>%
    apply(c(3, 2), t_test) %>%
    apply(2, p.adjust, method = adjust)
  meandiffs <- apply(x, c(3, 2), mean, na.rm = TRUE)

  model_names <- x@model_names
  num_models <- length(model_names)
  results <- array(NA, dim = c(num_models, num_models, size(x, 2)),
                   dimnames = list(Model2 = model_names,
                                   Model1 = model_names,
                                   Metric = dimnames(x)[[2]]))
  indices <- lower.tri(results[, , 1])
  results[indices] <- meandiffs
  results <- aperm(results, perm = c(2, 1, 3))
  results[indices] <- pvalues

  PerformanceDiffTest(results, adjust = adjust)
}
