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
#' have.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}}
#' \item{\link[=tune]{Automatic Tuning} Grid Parameters:}{
#'   \code{mtry}, \code{nodesize}*
#' }
#' }
#' * included only in randomly sampled grid points
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[randomForest]{randomForest}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = RandomForestModel())
#' 
RandomForestModel <- function(ntree = 500,
                              mtry = .(if (is.factor(y)) floor(sqrt(nvars))
                                       else max(floor(nvars / 3), 1)),
                              replace = TRUE,
                              nodesize = .(if (is.factor(y)) 1 else 5),
                              maxnodes = NULL) {
  
  MLModel(
    name = "RandomForestModel",
    label = "Random Forests",
    packages = "randomForest",
    response_types = c("factor", "numeric"),
    params = params(environment()),
    grid = function(x, length, random, ...) {
      params <- list(
        mtry = seq_nvars(x, RandomForestModel, length)
      )
      if (random) params$nodesize <- 1:min(nrow(x), 20)
      params
    },
    design = "terms",
    fit = function(formula, data, weights, ...) {
      assert_equal_weights(weights)
      eval_fit(data,
               formula = randomForest::randomForest(formula,
                                                    data = as.data.frame(data),
                                                    ...),
               matrix = randomForest::randomForest(x, y, ...))
    },
    predict = function(object, newdata, fitbits, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata,
              type = ifelse(is.factor(response(fitbits)), "prob", "response"))
    },
    varimp = function(object, ...) {
      randomForest::importance(object)
    }
  )
  
}
