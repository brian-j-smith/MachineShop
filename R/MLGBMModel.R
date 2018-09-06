#' Generalized Boosted Regression Model
#' 
#' Fits generalized boosted regression models.
#'
#' @param distribution either a character string specifying the name of the
#' distribution to use or a list with a component \code{name} specifying the
#' distribution and any additional parameters needed.
#' @param n.trees total number of trees to fit.
#' @param interaction.depth maximum depth of variable interactions.
#' @param n.minobsinnode minimum number of observations in the trees terminal
#' nodes.
#' @param shrinkage shrinkage parameter applied to each tree in the expansion.
#' @param bag.fraction fraction of the training set observations randomly
#' selected to propose the next tree in the expansion.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model
#' details can be found in the source link below.
#' 
#' @seealso \code{\link[gbm]{gbm}}
#'
GBMModel <- function(distribution = NULL, n.trees = NULL,
                     interaction.depth = NULL, n.minobsinnode = NULL,
                     shrinkage = NULL, bag.fraction = NULL) {
  MLModel(
    name = "GBMModel",
    packages = "gbm",
    types = c("factor", "numeric", "Surv"),
    params = params(environment()),
    fit = function(formula, data, weights = rep(1, nrow(data)), ...) {
      environment(formula) <- environment()
      args <- list(...)
      distribution <- args$distribution
      if(is.null(distribution)) {
        distribution <- switch(class(response(formula, data)),
                               "factor" = "multinomial",
                               "numeric" = "gaussian",
                               "Surv" = "coxph")
      }
      gbm::gbm(formula, data = data, distribution = distribution,
               weights = weights, ...) %>%
        asMLModelFit("GBMFit", GBMModel(...))
    },
    predict = function(object, newdata, times = numeric(), ...) {
      obs <- response(object)
      object <- asParentFit(object)
      if(object$distribution$name == "coxph") {
        if(length(times)) {
          lp <- predict(object, n.trees = object$n.trees, type = "link")
          newlp <- predict(object, newdata = newdata, n.trees = object$n.trees,
                           type = "link")
          cumhaz <- basehaz(obs, exp(lp), times)
          exp(exp(newlp - mean(lp)) %o% -cumhaz)
        } else {
          exp(predict(object, newdata = newdata, n.trees = object$n.trees,
                      type = "link"))
        }
      } else {
        predict(object, newdata = newdata, n.trees = object$n.trees,
                type = "response") %>% drop
      }
    },
    response = function(object, ...) {
      switch(object$distribution$name,
        "multinomial" = matrix(object$data$y, ncol = object$num.classes) %>%
          max.col %>%
          factor(levels = 1:object$num.classes, labels = object$classes),
        "coxph" = with(object$data, Surv(y, Misc)[order(i.timeorder),]),
        object$data$y
      )
    },
    varimp = function(object, n.trees = object$n.trees, ...) {
      gbm::relative.influence(object, n.trees = n.trees, ...)
    }
  )
}
