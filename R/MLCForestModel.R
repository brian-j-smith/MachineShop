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
#' Supplied arguments are passed to \code{\link[party:cforest_control]{cforest_unbiased}}.
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[party]{cforest}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#'
CForestModel <- function(teststat = NULL, testtype = NULL, mincriterion = NULL,
                         ntree = NULL, mtry = NULL, replace = NULL,
                         fraction = NULL) {
  args <- params(environment())
  MLModel(
    name = "CForestModel",
    packages = "party",
    types = c("factor", "numeric", "Surv"),
    params = list(controls = as.call(c(quote(party::cforest_unbiased), args))),
    fit = function(formula, data, weights, ...) {
      environment(formula) <- environment()
      party::cforest(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("CForestFit", do.call(CForestModel, args, quote = TRUE))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      object <- unMLModelFit(object)
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
          matrix(nrow = nrow(newdata), byrow = TRUE) %>%
          drop
      }
    },
    response = function(object, ...) {
      object@responses@variables[[1]]
    },
    varimp = function(object, ...) {
      party::varimp(object, ...)
    }
  )
}
