#' Conditional Random Forest Model
#' 
#' An implementation of the random forest and bagging ensemble algorithms
#' utilizing conditional inference trees as base learners.
#'
#' @param controls an object of class \code{\linkS4class{ForestControl}}.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[party]{cforest}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#'
CForestModel <- function(controls = NULL) {
  MLModel(
    name = "CForestModel",
    packages = "party",
    types = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      party::cforest(formula, data = data, weights = weights, ...) %>%
        asMLModelFit("CForestFit", CForestModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      object <- asParentFit(object)
      if(object@responses@is_censored) {
        if(length(times)) {
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
