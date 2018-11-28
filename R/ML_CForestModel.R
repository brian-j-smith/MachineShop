#' Conditional Random Forest Model
#' 
#' An implementation of the random forest and bagging ensemble algorithms
#' utilizing conditional inference trees as base learners.
#'
#' @param teststat character specifying the type of the test statistic to be
#' applied.
#' @param testtype character specifying how to compute the distribution of the
#' test statistic.
#' @param mincriterion  value of the test statistic that must be exceeded in
#' order to implement a split.
#' @param replace logical indicating whether sampling of observations is done
#' with or without replacement.
#' @param fraction fraction of number of observations to draw without
#' replacement (only relevant if \code{replace = FALSE}).
#' @param ntree number of trees to grow in a forest.
#' @param mtry number of input variables randomly sampled as candidates at each
#' node for random forest like algorithms.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#' }
#' 
#' Supplied arguments are passed to \code{\link[party]{cforest_control}}.
#' Further model details can be found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[party]{cforest}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' library(MASS)
#' 
#' fit(medv ~ ., data = Boston, model = CForestModel())
#'
CForestModel <- function(teststat = c("quad", "max"),
                         testtype = c("Univariate", "Teststatistic",
                                      "Bonferroni", "MonteCarlo"),
                         mincriterion = 0, ntree = 500, mtry = 5,
                         replace = TRUE, fraction = 0.632) {
  teststat <- match.arg(teststat)
  testtype <- match.arg(testtype)
  args <- params(environment())
  MLModel(
    name = "CForestModel",
    packages = "party",
    types = c("factor", "numeric", "Surv"),
    params = list(controls = as.call(c(.(party::cforest_control), args))),
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      party::cforest(formula, data = data, weights = weights, ...)
    },
    predict = function(object, newdata, times, ...) {
      if (object@responses@is_censored) {
        if (length(times)) {
          predict(object, newdata = newdata, type = "prob") %>%
            lapply(function(fit) predict(fit, times)) %>%
            (function(args) do.call(rbind, args))
        } else {
          log(2) / predict(object, newdata = newdata, type = "response")
        }
      } else {
        predict(object, newdata = newdata, type = "prob") %>%
          unlist %>%
          matrix(nrow = nrow(newdata), byrow = TRUE)
      }
    },
    varimp = function(object, ...) {
      party::varimp(object, ...)
    }
  )
}
