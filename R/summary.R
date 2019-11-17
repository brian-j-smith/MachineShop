#' Model Performance Summary
#' 
#' Summary statistics for resampled model performance metrics.
#' 
#' @name summary
#' @rdname summary-methods
#' 
#' @param object \link{confusion}, \link[=performance_curve]{performance curve},
#'   \link{lift}, tuned model \link{fit}, \link{performance}, or \link{resample}
#'   result.
#' @param stat function or character string naming a function to compute a
#'   summary statistic at each cutoff value of resampled metrics in
#'   \code{Curves}, or \code{NULL} for resample-specific metrics.
#' @param stats function, function name, or vector of these with which to
#'   compute summary statistics.
#' @param na.rm logical indicating whether to exclude missing values.
#' @param ... arguments passed to other methods.
#' 
#' @return array with summmary statistics in the second dimension, metrics in
#' the first for single models, and models and metrics in the first and third,
#' respectively, for multiple models.
#' 
#' @examples
#' ## Factor response example
#' 
#' fo <- Species ~ .
#' control <- CVControl()
#' 
#' gbm_res1 <- resample(fo, iris, GBMModel(n.trees = 25), control)
#' gbm_res2 <- resample(fo, iris, GBMModel(n.trees = 50), control)
#' gbm_res3 <- resample(fo, iris, GBMModel(n.trees = 100), control)
#' summary(gbm_res3)
#' 
#' res <- c(GBM1 = gbm_res1, GBM2 = gbm_res2, GBM3 = gbm_res3)
#' summary(res)
#' 
NULL


#' @rdname summary-methods
#' 
summary.ConfusionList <- function(object, ...) {
  ListOf(lapply(object, summary, ...))
}


#' @rdname summary-methods
#' 
summary.ConfusionMatrix <- function(object, ...) {
  total <- sum(object)
  object <- object / total
  
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
  
  ConfusionSummary(perf,
                   total = total,
                   accuracy = sum(agreement),
                   majority = max(observed),
                   kappa2 = 1 - (1 - sum(agreement)) /
                     (1 - sum(observed * predicted))) 
}


#' @rdname summary-methods
#' 
summary.Curves <- function(object, stat = MachineShop::settings("stat.Curves"),
                           ...) {
  if (!(is.null(object$Resample) || is.null(stat))) {
    
    stat <- fget(stat)
    
    object_class <- class(object)
    stat_na_omit <- function(x) stat(na.omit(x))
    
    object_list <- by(object, object$Model, function(curves) {
      cutoffs <- unique(curves$Cutoff)
      curves_split <- split(curves, curves$Resample)
      x_all <- y_all <- matrix(NA, length(cutoffs), length(curves_split))
      for (j in seq(curves_split)) {
        curve <- curves_split[[j]]
        x_all[, j] <- .curve_approx(curve$Cutoff, curve$x, cutoffs)
        y_all[, j] <- .curve_approx(curve$Cutoff, curve$y, cutoffs)
      }
      do.call(object_class, list(
        data.frame(Cutoff = cutoffs,
                   x = apply(x_all, 1, stat_na_omit),
                   y = apply(y_all, 1, stat_na_omit)),
        metrics = object@metrics
      ))
    })
    
    object <- do.call(c, object_list)
  }
  
  object
}


.curve_approx <- function(...) {
  values <- try(
    approx(..., method = "constant", rule = 2, f = 0.5),
    silent = TRUE
  )
  if (is(values, "try-error")) NA else values$y
}


#' @rdname summary-methods
#' 
summary.MLModel <- function(object, stats =
                              MachineShop::settings("stats.Resamples"),
                            na.rm = TRUE, ...) {
  if (is.null(object@tune)) stop("no tuning results to summarize")
  summary(object@tune@performance, stats = stats, na.rm = na.rm, ...)
}


summary.MLModelFit <- function(object, ...) {
  summary(unMLModelFit(object))
}


#' @rdname summary-methods
#' 
summary.Performance <- function(object, stats =
                                  MachineShop::settings("stats.Resamples"),
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
summary.Resamples <- function(object, stats =
                                MachineShop::settings("stats.Resamples"),
                              na.rm = TRUE, ...) {
  summary(performance(object), stats = stats, na.rm = na.rm)
}
