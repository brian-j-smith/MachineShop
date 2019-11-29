#' Generalized Boosted Regression Model
#'
#' Fits generalized boosted regression models.
#'
#' @param distribution optional character string specifying the name of the
#'   distribution to use or list with a component \code{name} specifying the
#'   distribution and any additional parameters needed.  Set automatically
#'   according to the class type of the response variable.
#' @param n.trees total number of trees to fit.
#' @param interaction.depth maximum depth of variable interactions.
#' @param n.minobsinnode minimum number of observations in the trees terminal
#'   nodes.
#' @param shrinkage shrinkage parameter applied to each tree in the expansion.
#' @param bag.fraction fraction of the training set observations randomly
#'   selected to propose the next tree in the expansion.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{n.trees}, \code{interaction.depth}, \code{shrinkage}*,
#'     \code{n.minobsinnode}*
#'   }
#' }
#' * included only in randomly sampled grid points
#'
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[gbm]{gbm}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' fit(Species ~ ., data = iris, model = GBMModel)
#'
GBMModel <- function(distribution = NULL, n.trees = 100,
                     interaction.depth = 1, n.minobsinnode = 10,
                     shrinkage = 0.1, bag.fraction = 0.5) {

  MLModel(
    name = "GBMModel",
    label = "Generalized Boosted Regression",
    packages = "gbm",
    response_types = c("factor", "numeric", "PoissonVector", "Surv"),
    predictor_encoding = "terms",
    params = params(environment()),
    grid = function(x, length, random, ...) {
      params <- list(
        n.trees = round(seq_range(0, 50, c(1, 1000), length + 1)),
        interaction.depth = 1:min(length, 10)
      )
      if (random) {
        params$shrinkage <- seq(0.001, 0.1, length = length)
        params$n.minobsinnode <-
          round(seq(1, min(20, nrow(x)), length = length))
      }
      params
    },
    fit = function(formula, data, weights, distribution = NULL, ...) {
      if (is.null(distribution)) {
        distribution <- switch_class(response(data),
                                     factor = "multinomial",
                                     numeric = "gaussian",
                                     PoissonVector = "poisson",
                                     Surv = "coxph")
      }
      eval_fit(data,
               formula = gbm::gbm(formula, data = as.data.frame(data),
                                  weights = weights,
                                  distribution = distribution, ...),
               matrix = gbm::gbm.fit(x, y, offset = model.offset(data),
                                     w = weights, distribution = distribution,
                                     verbose = FALSE, ...))
    },
    predict = function(object, newdata, model, times, ...) {
      newdata <- as.data.frame(newdata)
      n <- object$n.trees
      if (object$distribution$name == "coxph") {
        y <- response(model)
        data <- preprocess(model@x)
        lp <- predict(object, newdata = data, n.trees = n, type = "link")
        new_lp <- predict(object, newdata = newdata, n.trees = n, type = "link")
        predict(y, lp, times, new_lp, ...)
      } else {
        predict(object, newdata = newdata, n.trees = n, type = "response")
      }
    },
    varimp = function(object, ...) {
      gbm::relative.influence(object, n.trees = object$n.trees)
    }
  )

}

MLModelFunction(GBMModel) <- NULL
