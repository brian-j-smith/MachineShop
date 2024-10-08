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
#'   \item{Response types:}{\code{factor}, \code{numeric},
#'     \code{PoissonVariate}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{n.trees}, \code{interaction.depth}, \code{shrinkage}*,
#'     \code{n.minobsinnode}*
#'   }
#' }
#' * excluded from grids by default
#'
#' Default argument values and further model details can be found in the source
#' See Also link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[gbm]{gbm}}, \code{\link{fit}}, \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package gbm to run
#'
#' fit(Species ~ ., data = iris, model = GBMModel)
#' }
#'
GBMModel <- function(
  distribution = character(), n.trees = 100, interaction.depth = 1,
  n.minobsinnode = 10, shrinkage = 0.1, bag.fraction = 0.5
) {

  MLModel(

    name = "GBMModel",
    label = "Generalized Boosted Regression",
    packages = "gbm",
    response_types = c("factor", "numeric", "PoissonVariate", "Surv"),
    weights = TRUE,
    predictor_encoding = "model.frame",
    na.rm = "response",
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode"),
      get_values = c(
        function(n, ...) round_int(seq_range(0, 50, c(1, 1000), n + 1)),
        function(n, ...) seq_len(min(n, 10)),
        function(n, ...) seq(0.001, 0.1, length = n),
        function(n, data, ...) {
          round_int(seq(1, min(20, nrow(data)), length = n))
        }
      ),
      default = c(TRUE, TRUE, FALSE, FALSE)
    ),

    fit = function(formula, data, weights, distribution = NULL, ...) {
      if (is.null(distribution)) {
        y <- response(data)
        distribution <- switch_class(y,
          "factor" = if (nlevels(y) <= 2) {
            data[[response(formula)]] <- as.numeric(y) - 1
            "bernoulli"
          } else {
            data[[response(formula)]] <- structure(y, class = "factor")
            "multinomial"
          },
          "numeric" = "gaussian",
          "PoissonVariate" = "poisson",
          "Surv" = "coxph"
        )
      }
      cond <- NULL
      suppressWarnings(withCallingHandlers(
        res <- eval_fit(
          data,
          formula = gbm::gbm(
            formula, data = data, weights = weights,
            distribution = distribution, ...
          ),
          matrix = gbm::gbm.fit(
            x, y, offset = model.offset(data), w = weights,
            distribution = distribution, verbose = FALSE, ...
          )
        ),
        warning = function(cond) cond <<- cond
      ))
      if (is(cond, "warning")) {
        if (distribution == "multinomial") {
          throw(Warning(conditionMessage(cond)), call = call("gbm"), times = 3)
        } else {
          warning(cond)
        }
      }
      res
    },

    predict = function(object, newdata, .MachineShop, ...) {
      input <- .MachineShop$input
      newdata <- as.data.frame(newdata)
      n <- object$n.trees
      if (object$distribution$name == "coxph") {
        y <- response(input)
        data <- as.data.frame(PredictorFrame(input))
        lp <- predict(object, newdata = data, n.trees = n, type = "link")
        new_lp <- predict(object, newdata = newdata, n.trees = n, type = "link")
        predict(y, lp, new_lp, weights = case_weights(input), ...)
      } else {
        predict(object, newdata = newdata, n.trees = n, type = "response")
      }
    },

    varimp = function(object, ...) {
      structure(
        gbm::relative.influence(object, n.trees = object$n.trees),
        metric = "influence"
      )
    }

  )

}

MLModelFunction(GBMModel) <- NULL
