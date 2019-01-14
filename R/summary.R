#' Model Performance Summary
#' 
#' Summary statistics for resampled model performance metrics.
#' 
#' @name summary
#' @rdname summary-methods
#' 
#' @param object object to summarize.
#' @param stats function, one or more function names, or list of named functions
#' to include in the calculation of summary statistics.
#' @param na.rm logical indicating whether to exclude missing values.
#' @param ... arguments passed to other methods.
#' 
#' @return array with summmary statistics in the second dimension, metrics in
#' the first for single models, and models and metrics in the first and third,
#' respectively, for multiple models.
#' 
#' @seealso \code{\link{performance}}, \code{\link{resample}},
#' \code{\link{diff}}, \code{\link{tune}}, \code{\link{confusion}}
#' 
summary.Performance <- function(object,
                                stats = c("Mean" = base::mean,
                                          "Median" = stats::median,
                                          "SD" = stats::sd,
                                          "Min" = base::min,
                                          "Max" = base::max),
                                na.rm = TRUE, ...) {
  stats <- list2function(stats)

  f <- function(x) {
    prop_na <- mean(is.na(x))
    if (na.rm) x <- as.numeric(na.omit(x))
    c(stats(x), "NA" = prop_na)
  }
  
  margins <- 2
  perm <- c(2, 1)
  if (length(dim(object)) > 2) {
    margins <- c(3, margins)
    perm <- c(perm, 3)
  }
  aperm(apply(object, margins, f), perm = perm)
}


#' @rdname summary-methods
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- Species ~ .
#' control <- CVControl()
#' 
#' gbmres1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbmres2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbmres3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' summary(gbmres3)
#' 
#' res <- Resamples(GBM1 = gbmres1, GBM2 = gbmres2, GBM3 = gbmres3)
#' summary(res)
#' 
summary.Resamples <- function(object,
                              stats = c("Mean" = base::mean,
                                        "Median" = stats::median,
                                        "SD" = stats::sd,
                                        "Min" = base::min,
                                        "Max" = base::max),
                              na.rm = TRUE, ...) {
  summary(performance(object), stats = stats, na.rm = na.rm)
}


#' @rdname summary-methods
#' 
summary.MLModelTune <- function(object,
                                stats = c("Mean" = base::mean,
                                          "Median" = stats::median,
                                          "SD" = stats::sd,
                                          "Min" = base::min,
                                          "Max" = base::max),
                                na.rm = TRUE, ...) {
  summary(object@performance, stats = stats, na.rm = na.rm, ...)
}


#' @rdname summary-methods
#' 
summary.Confusion <- function(object, ...) {
  structure(lapply(object, summary), class = "listof")
}


#' @rdname summary-methods
#' 
summary.ConfusionMatrix <- function(object, ...) {
  n <- sum(object)
  object <- object / n
  
  observed <- colSums(object)
  predicted <- rowSums(object)
  agreement <- diag(object)
  
  perf <- rbind(
    Observed = observed,
    Predicted = predicted,
    Agreement = agreement,
    Sensitivity = agreement / observed,
    Specificity = (1 - observed - predicted + agreement) / (1 - observed),
    PPV = agreement / predicted,
    NPV = (1 - observed - predicted + agreement) / (1 - predicted)
  )
  
  SummaryConfusion(perf,
                   N = n,
                   Accuracy = sum(agreement),
                   Majority = max(observed),
                   Kappa = 1 - (1 - sum(agreement)) /
                     (1 - sum(observed * predicted))) 
}


summary.MLModelFit <- function(object, ...) {
  summary(unMLModelFit(object))
}
