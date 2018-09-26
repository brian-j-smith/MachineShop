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
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[gbm]{gbm}}, \code{\link{fit}}, \code{\link{resample}},
#' \code{\link{tune}}
#'
GBMModel <- function(distribution = NULL, n.trees = 100,
                     interaction.depth = 1, n.minobsinnode = 10,
                     shrinkage = 0.1, bag.fraction = 0.5) {
  MLModel(
    name = "GBMModel",
    packages = "gbm",
    types = c("factor", "numeric", "Surv"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "terms"),
    fit = function(formula, data, weights, distribution = NULL, ...) {
      environment(formula) <- environment()
      if (is.null(distribution)) {
        distribution <- switch_class(response(formula, data),
                                     "factor" = "multinomial",
                                     "numeric" = "gaussian",
                                     "Surv" = "coxph")
      }
      gbm::gbm(formula, data = data, weights = weights,
               distribution = distribution, ...)
    },
    predict = function(object, newdata, times = numeric(), ...) {
      obs <- response(object)
      object <- unMLModelFit(object)
      if (object$distribution$name == "coxph") {
        if (length(times)) {
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
