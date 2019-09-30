#' Multivariate Adaptive Regression Splines Model
#' 
#' Build a regression model using the techniques in Friedman's papers
#' "Multivariate Adaptive Regression Splines" and "Fast MARS".
#' 
#' @param pmethod pruning method.
#' @param trace level of execution information to display.
#' @param degree maximum degree of interaction.
#' @param nprune maximum number of terms (including intercept) in the pruned
#'   model.
#' @param nfold number of cross-validation folds.
#' @param ncross number of cross-validations if \code{nfold > 1}.
#' @param stratify logical indicating whether to stratify cross-validation
#'   samples by the response levels.
#' 
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{nprune}, \code{degree}*
#'   }
#' }
#' * included only in randomly sampled grid points
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' In calls to \code{\link{varimp}} for \code{EarthModel}, argument
#' \code{metric} may be specified as \code{"gcv"} (default) for the generalized
#' cross-validation decrease over all subsets that include each predictor, as
#' \code{"rss"} for the residual sums of squares decrease, or as
#' \code{"nsubsets"} for the number of model subsets that include each
#' predictor.  Variable importance is automatically scaled to range from 0 to
#' 100.  To obtain unscaled importance values, set \code{scale = FALSE}.  See
#' example below.
#' 
#' @return \code{MLModel} class object.
#' 
#' @seealso \code{\link[earth]{earth}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#' 
#' @examples
#' model_fit <- fit(Species ~ ., data = iris, model = EarthModel)
#' varimp(model_fit, metric = "nsubsets", scale = FALSE)
#' 
EarthModel <- function(pmethod = c("backward", "none", "exhaustive", "forward",
                                   "seqrep", "cv"),
                       trace = 0, degree = 1, nprune = NULL,
                       nfold = 0, ncross = 1, stratify = TRUE) {
  
  pmethod <- match.arg(pmethod)
  
  MLModel(
    name = "EarthModel",
    label = "Multivariate Adaptive Regression Splines",
    packages = "earth",
    response_types = c("factor", "numeric"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    grid = function(x, length, random, ...) {
      modelfit <- fit(x, model = EarthModel(pmethod = "none"))
      max_terms <- min(2 + 0.75 * nrow(modelfit$dirs), 200)
      params <- list(
        nprune = round(seq(2, max_terms, length = length))
      )
      if (random) params$degree <- 1:2
      params
    },
    fit = function(formula, data, weights, ...) {
      attachment(list(
        contr.earth.response = earth::contr.earth.response
      ), name = "earth_exports")
      
      glm <- list(family = switch_class(response(data),
                                        "factor" = "binomial",
                                        "numeric" = "gaussian"))
      eval_fit(data,
               formula = earth::earth(formula, data = as.data.frame(data),
                                      weights = weights, glm = glm, ...),
               matrix = earth::earth(x, y, weights = weights, glm = glm, ...))
    },
    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "response")
    },
    varimp = function(object, metric = c("gcv", "rss", "nsubsets"), ...) {
      earth::evimp(object)[, match.arg(metric), drop = FALSE]
    }
  )
  
}
