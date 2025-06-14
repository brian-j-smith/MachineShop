#' Random Forest Model
#'
#' Implementation of Breiman's random forest algorithm (based on Breiman and
#' Cutler's original Fortran code) for classification and regression.
#'
#' @param ntree number of trees to grow.
#' @param mtry number of variables randomly sampled as candidates at each split.
#' @param replace should sampling of cases be done with or without replacement?
#' @param nodesize minimum size of terminal nodes.
#' @param maxnodes maximum number of terminal nodes trees in the forest can
#'   have.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}, \code{numeric}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{mtry}, \code{nodesize}*
#'   }
#' }
#' * excluded from grids by default
#'
#' Default argument values and further model details can be found in the source
#' See Also link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[randomForest]{randomForest}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package randomForest to run
#'
#' fit(sale_amount ~ ., data = ICHomes, model = RandomForestModel)
#' }
#'
RandomForestModel <- function(
  ntree = 500,
  mtry = .(if (is.factor(y)) floor(sqrt(nvars)) else max(floor(nvars / 3), 1)),
  replace = TRUE, nodesize = .(if (is.factor(y)) 1 else 5), maxnodes = integer()
) {

  MLModel(

    name = "RandomForestModel",
    label = "Random Forests",
    packages = "randomForest",
    response_types = c("factor", "numeric"),
    predictor_encoding = "model.frame",
    na.rm = TRUE,
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("mtry", "nodesize"),
      get_values = c(
        function(n, data, ...) seq_nvars(data, RandomForestModel, n),
        function(n, data, ...) {
          round_int(seq(1, min(20, nrow(data)), length = n))
        }
      ),
      default = c(TRUE, FALSE)
    ),

    fit = function(formula, data, weights, ...) {
      eval_fit(
        data,
        formula = randomForest::randomForest(
          formula, data = data, na.action = na.pass, ...
        ),
        matrix = randomForest::randomForest(x, y, ...)
      )
    },

    predict = function(object, newdata, .MachineShop, ...) {
      y <- response(.MachineShop$input)
      newdata <- as.data.frame(newdata)
      predict(
        object, newdata = newdata,
        type = if (is.factor(y)) "prob" else "response"
      )
    },

    varimp = function(object, .MachineShop, ...) {
      y <- response(.MachineShop$input)
      vi <- randomForest::importance(object)
      structure(
        vi[, 1], names = rownames(vi),
        metric = if (is.factor(y)) "gini" else "rss/sse"
      )
    }

  )

}

MLModelFunction(RandomForestModel) <- NULL
