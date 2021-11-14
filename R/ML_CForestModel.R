#' Conditional Random Forest Model
#'
#' An implementation of the random forest and bagging ensemble algorithms
#' utilizing conditional inference trees as base learners.
#'
#' @param teststat character specifying the type of the test statistic to be
#'   applied.
#' @param testtype character specifying how to compute the distribution of the
#'   test statistic.
#' @param mincriterion  value of the test statistic that must be exceeded in
#'   order to implement a split.
#' @param replace logical indicating whether sampling of observations is done
#'   with or without replacement.
#' @param fraction fraction of number of observations to draw without
#'   replacement (only relevant if \code{replace = FALSE}).
#' @param ntree number of trees to grow in a forest.
#' @param mtry number of input variables randomly sampled as candidates at each
#'   node for random forest like algorithms.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters:}{
#'     \code{mtry}
#'   }
#' }
#'
#' Supplied arguments are passed to \code{\link[party]{cforest_control}}.
#' Further model details can be found in the source link below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[party]{cforest}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' fit(sale_amount ~ ., data = ICHomes, model = CForestModel)
#'
CForestModel <- function(
  teststat = c("quad", "max"),
  testtype = c("Univariate", "Teststatistic", "Bonferroni", "MonteCarlo"),
  mincriterion = 0, ntree = 500, mtry = 5, replace = TRUE, fraction = 0.632
) {

  teststat <- match.arg(teststat)
  testtype <- match.arg(testtype)

  MLModel(

    name = "CForestModel",
    label = "Conditional Random Forests",
    packages = "party",
    response_types = c("factor", "numeric", "Surv"),
    weights = TRUE,
    predictor_encoding = "model.frame",
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = "mtry",
      get_values = c(
        function(n, data, ...) seq_nvars(data, CForestModel, n)
      )
    ),

    fit = function(formula, data, weights, ...) {
      party::cforest(formula, data = as.data.frame(data), weights = weights,
                     controls = party::cforest_control(...))
    },

    predict = function(object, newdata, model, ...) {
      newdata <- as.data.frame(newdata)
      if (object@responses@is_censored) {
        y <- response(model)
        fits <- predict(object, newdata = newdata, type = "prob")
        predict(y, fits, ...)
      } else {
        predict(object, newdata = newdata, type = "prob") %>%
          unlist %>%
          matrix(nrow = nrow(newdata), byrow = TRUE)
      }
    },

    varimp = function(object, ...) {
      party::varimp(object, ...)
    }

  )

}

MLModelFunction(CForestModel) <- NULL
