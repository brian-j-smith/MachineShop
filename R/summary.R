#' Model Performance Summary
#' 
#' Summary statistics for resampled model performance metrics.
#' 
#' @name summary
#' @rdname summary-methods
#' 
#' @param object object to summarize.
#' @param stats function or list of named functions to include in the
#' calculation of summary statistics.
#' @param na.rm logical indicating whether to exclude missing values.
#' @param ... arguments passed to other methods.
#' 
#' @return array with summmary statistics in the second dimension, metrics in
#' the first for single models, and models and metrics in the first and third,
#' respectively, for multiple models.
#' 
#' @seealso \code{\link{diff}}, \code{\link{modelmetrics}}
#' 
summary.ModelMetrics <- function(object,
                                 stats = c("Mean" = mean,
                                           "Median" = median,
                                           "SD" = sd,
                                           "Min" = min,
                                           "Max" = max),
                                 na.rm = TRUE, ...) {
  if (is.list(stats)) {
    stats <- eval(bquote(
      function(x) sapply(.(stats), function(stat) stat(x))
    ))
  }
  
  f <- function(x) {
    prop_na <- mean(is.na(x))
    if (na.rm) x <- na.omit(x)
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
#' @seealso \code{\link{resample}}, \code{\link{Resamples}}
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
                              stats = c("Mean" = mean,
                                        "Median" = median,
                                        "SD" = sd,
                                        "Min" = min,
                                        "Max" = max),
                              na.rm = TRUE, ...) {
  summary(modelmetrics(object), stats = stats, na.rm = na.rm)
}


#' @rdname summary-methods
#' 
#' @seealso \code{\link{tune}}
#' 
summary.MLModelTune <- function(object,
                                stats = c("Mean" = mean,
                                          "Median" = median,
                                          "SD" = sd,
                                          "Min" = min,
                                          "Max" = max),
                                na.rm = TRUE, ...) {
  summary(object@resamples, stats = stats, na.rm = na.rm, ...)
}


#' @rdname summary-methods
#' 
#' @seealso \code{\link{confusion}}
#' 
summary.ConfusionResamples <- function(object, ...) {
  lapply(object, function(conf) {
    n <- sum(conf)
    conf <- conf / n
    
    observed <- colSums(conf)
    predicted <- rowSums(conf)
    agreement <- diag(conf)
    
    metrics <- rbind(
      Observed = observed,
      Predicted = predicted,
      Agreement = agreement,
      Sensitivity = agreement / observed,
      Specificity = (1 - observed - predicted + agreement) / (1 - observed),
      PPV = agreement / predicted,
      NPV = (1 - observed - predicted + agreement) / (1 - predicted)
    )
    
    SummaryConfusion(metrics,
                     N = n,
                     Accuracy = sum(agreement),
                     Majority = max(observed),
                     Kappa = 1 - (1 - sum(agreement)) /
                       (1 - sum(observed * predicted))) 
  }) %>% structure(class = "listof")
}


summary.MLControl <- function(object, observed, predicted, ...) {
  if (object@na.rm) {
    df <- data.frame(
      observed = I(observed),
      predicted = I(predicted)
    ) %>% na.omit
    observed <- unAsIs(df$observed)
    predicted <- unAsIs(df$predicted)
  }
  do.call(object@summary, c(list(observed, predicted), as(object, "list")))
}


summary.MLModelFit <- function(object, ...) {
  summary(unMLModelFit(object))
}
