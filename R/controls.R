setClass("CVControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "AbstractControl")

CVControl <- function(...) new("CVControl", ...)

setMethod("initialize", "CVControl",
  function(.Object, folds = 10, repeats = 1, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@folds <- folds
    .Object@repeats <- repeats
    .Object
  }
)


setGeneric("resampleSummary", function(observed, predicted, ...) {
  standardGeneric("resampleSummary")
})


setMethod("resampleSummary", c("factor", "factor"),
  function(observed, predicted, ...) {
    ratings <- cbind(observed, predicted)
    c("Accuracy" = 1 - ce(observed, predicted),
      "Kappa" = kappa2(ratings, weight = "unweighted")$value,
      "WeightedKappa" = kappa2(ratings, weight = "equal")$value)
  }
)


setMethod("resampleSummary", c("factor", "matrix"),
  function(observed, predicted, ...) {
    n <- nlevels(observed)
    predicted <- if(n > 2) {
      factor(max.col(predicted), levels = 1:n, labels = levels(observed))
    } else {
      predicted[,n]
    }
    resampleSummary(observed, predicted, ...)
  }
)


setMethod("resampleSummary", c("factor", "numeric"),
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


setMethod("resampleSummary", c("numeric"),
  function(observed, predicted, ...) {
    c("RMSE" = rmse(observed, predicted),
      "RSquare" = cor(observed, predicted, use = "pairwise.complete.obs")^2,
      "MAE" = mae(observed, predicted))
  }
)


setMethod("resampleSummary", c("Surv", "matrix"),
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


setMethod("resampleSummary", c("Surv", "numeric"),
  function(observed, predicted, ...) {
    c("CIndex" = rcorr.cens(-predicted, observed)[[1]])
  }
)
