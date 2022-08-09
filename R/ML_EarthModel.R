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
#'   \item{Response types:}{\code{factor}, \code{numeric}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{nprune}, \code{degree}*
#'   }
#' }
#' * excluded from grids by default
#'
#' Default values and further model details can be found in the source link
#' below.
#'
#' In calls to \code{\link{varimp}} for \code{EarthModel}, argument
#' \code{type} may be specified as \code{"nsubsets"} (default) for the number of
#' model subsets that include each predictor, as \code{"gcv"} for the
#' generalized cross-validation decrease over all subsets that include each
#' predictor, or as \code{"rss"} for the residual sums of squares decrease.
#' Variable importance is automatically scaled to range from 0 to 100.  To
#' obtain unscaled importance values, set \code{scale = FALSE}.  See example
#' below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[earth]{earth}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package earth to run
#'
#' model_fit <- fit(Species ~ ., data = iris, model = EarthModel)
#' varimp(model_fit, method = "model", type = "gcv", scale = FALSE)
#' }
#'
EarthModel <- function(
  pmethod = c("backward", "none", "exhaustive", "forward", "seqrep", "cv"),
  trace = 0, degree = 1, nprune = integer(), nfold = 0, ncross = 1,
  stratify = TRUE
) {

  pmethod <- match.arg(pmethod)

  MLModel(

    name = "EarthModel",
    label = "Multivariate Adaptive Regression Splines",
    packages = "earth",
    response_types = c("factor", "numeric"),
    weights = TRUE,
    predictor_encoding = "model.matrix",
    na.rm = TRUE,
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("nprune", "degree"),
      get_values = c(
        function(n, data, ...) {
          model_fit <- fit(data, model = EarthModel(pmethod = "none"))
          max_terms <- min(2 + 0.75 * nrow(model_fit$dirs), 200)
          round_int(seq(2, max_terms, length = n))
        },
        function(n, ...) head(1:2, n)
      ),
      default = c(TRUE, FALSE)
    ),

    fit = function(formula, data, weights, ...) {
      attach_objects(list(
        contr.earth.response = earth::contr.earth.response
      ), name = "earth_exports")

      glm <- list(
        family = switch_class(response(data),
          "factor" = "binomial",
          "numeric" = "gaussian"
        )
      )
      eval_fit(
        data,
        formula = earth::earth(
          formula, data = data, weights = weights, glm = glm, ...
        ),
        matrix = earth::earth(x, y, weights = weights, glm = glm, ...)
      )
    },

    predict = function(object, newdata, ...) {
      newdata <- as.data.frame(newdata)
      predict(object, newdata = newdata, type = "response")
    },

    varimp = function(object, type = c("nsubsets", "gcv", "rss"), ...) {
      earth::evimp(object)[, match.arg(type), drop = FALSE]
    }

  )

}

MLModelFunction(EarthModel) <- NULL
