AbstractControl <- R6Class(
  "AbstractControl",
  list(
    summary = NULL,
    cutoff = NULL,
    cutoff.index = NULL,
    survtimes = NULL,
    initialize = function(summary = validateSummary, cutoff = 0.5,
                          cutoff.index = function(sens, spec) sens + spec,
                          survtimes = NULL) {
      self$summary <- summary
      self$cutoff <- cutoff
      self$cutoff.index <- cutoff.index
      self$survtimes <- sort(unique(survtimes))
      self
    }
  )
)


CVControl <- R6Class(
  "CVControl",
  inherit = AbstractControl,
  list(
    folds = NULL,
    repeats = NULL,
    initialize = function(folds = 10, repeats = 1, ...) {
      super$initialize(...)
      self$folds <- folds
      self$repeats <- repeats
      self
    }
  )
)


setGeneric("validateSummary", function(observed, predicted, ...) {
  standardGeneric("validateSummary")
})


setMethod("validateSummary", c("factor", "factor"),
  function(observed, predicted, ...) {
    ratings <- cbind(observed, predicted)
    c("Accuracy" = 1 - ce(observed, predicted),
      "Kappa" = kappa2(ratings, weight = "unweighted")$value,
      "WeightedKappa" = kappa2(ratings, weight = "equal")$value)
  }
)


setMethod("validateSummary", c("factor", "matrix"),
  function(observed, predicted, ...) {
    n <- nlevels(observed)
    predicted <- if(n > 2) {
      factor(max.col(predicted), levels = 1:n, labels = levels(observed))
    } else {
      predicted[,2]
    }
    validateSummary(observed, predicted, ...)
  }
)


setMethod("validateSummary", c("factor", "numeric"),
  function(observed, predicted, cutoff, cutoff.index, ...) {
    observed <- observed == levels(observed)[2]
    sens <- sensitivity(observed, predicted, cutoff)
    spec <- specificity(observed, predicted, cutoff)
    c("Accuracy" = 1 - ce(observed, predicted > cutoff),
      "Kappa" = kappa(observed, predicted, cutoff),
      "Brier" = brier(observed, predicted),
      "ROC" = auc(observed, predicted),
      "Sensitivity" = sens,
      "Specificity" = spec,
      "Index" = cutoff.index(sens, spec))
  }
)


setMethod("validateSummary", c("numeric"),
  function(observed, predicted, ...) {
    c("RMSE" = rmse(observed, predicted),
      "RSquare" = cor(observed, predicted, use = "pairwise.complete.obs")^2,
      "MAE" = mae(observed, predicted))
  }
)


setMethod("validateSummary", c("Surv", "matrix"),
  function(observed, predicted, survtimes, ...) {
    ntimes <- length(survtimes)
    roc <- brier <- rep(NA, ntimes)
    for(i in 1:ntimes) {
      roc[i] <- rocSurv(observed, predicted[,i], survtimes[i])
      brier[i] <- brierSurv(observed, predicted[,i], survtimes[i])
    }
    statnames <- c("ROC", "Brier")
    if(ntimes > 1) {
      roc <- c(meanSurvMetric(roc, survtimes), roc)
      brier <- c(meanSurvMetric(brier, survtimes), brier)
      statnames <- c(statnames, paste(rep(statnames, each = ntimes),
                                      1:ntimes, sep = ".t"))
    }
    structure(c(roc[1], brier[1], roc[-1], brier[-1]), names = statnames)
  }
)


setMethod("validateSummary", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    c("CIndex" = rcorr.cens(-predicted, observed)[[1]])
  }
)
