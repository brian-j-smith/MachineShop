#' Gradient Boosting with Additive Models
#'
#' Gradient boosting for optimizing arbitrary loss functions, where
#' component-wise arbitrary base-learners, e.g., smoothing procedures, are
#' utilized as additive base-learners.
#'
#' @param family optional \code{\link[mboost]{Family}} object.  Set
#'   automatically according to the class type of the response variable.
#' @param baselearner character specifying the component-wise
#'   \code{\link[mboost:baselearners]{base learner}} to be used.
#' @param dfbase gobal degrees of freedom for P-spline base learners
#'   (\code{"bbs"}).
#' @param mstop number of initial boosting iterations.
#' @param nu step size or shrinkage parameter between 0 and 1.
#' @param risk method to use in computing the empirical risk for each boosting
#'   iteration.
#' @param stopintern logical inidicating whether the boosting algorithm stops
#'   internally when the out-of-bag risk increases at a subsequent iteration.
#' @param trace logical indicating whether status information is printed during
#'   the fitting process.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{binary factor}, \code{BinomialVariate},
#'     \code{NegBinomialVariate}, \code{numeric}, \code{PoissonVariate},
#'     \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameter:}{
#'     \code{mstop}
#'   }
#' }
#'
#' Default argument values and further model details can be found in the source
#' See Also links below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[mboost]{gamboost}}, \code{\link[mboost]{Family}},
#' \code{\link[mboost]{baselearners}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package mboost to run
#'
#' data(Pima.tr, package = "MASS")
#'
#' fit(type ~ ., data = Pima.tr, model = GAMBoostModel)
#' }
#'
GAMBoostModel <- function(
  family = NULL, baselearner = c("bbs", "bols", "btree", "bss", "bns"),
  dfbase = 4, mstop = 100, nu = 0.1, risk = c("inbag", "oobag", "none"),
  stopintern = FALSE, trace = FALSE
) {

  baselearner <- match.arg(baselearner)
  risk <- match.arg(risk)

  MLModel(

    name = "GAMBoostModel",
    label = "Gradient Boosting with Additive Models",
    packages = "mboost",
    response_types = c(
      "binary", "BinomialVariate", "NegBinomialVariate", "numeric",
      "PoissonVariate", "Surv"
    ),
    weights = TRUE,
    predictor_encoding = "model.frame",
    na.rm = if (baselearner == "btree") "response" else "all",
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = "mstop",
      get_values = c(
        function(n, ...) round_int(seq_range(0, 50, c(1, 1000), n + 1))
      )
    ),

    fit = function(
      formula, data, weights, family = NULL, baselearner, dfbase, ...
    ) {
      attach_objects(list(
        bbs = mboost::bbs,
        bols = mboost::bols,
        btree = mboost::btree,
        bbs = mboost::bbs,
        bns = mboost::bns
      ), name = "mboost_exports")

      if (is.null(family)) {
        family <- switch_class(response(data),
          "BinomialVariate" = mboost::Binomial(type = "glm"),
          "factor" = mboost::Binomial(),
          "NegBinomialVariate" = mboost::NBinomial(),
          "numeric" = mboost::Gaussian(),
          "PoissonVariate" = mboost::Poisson(),
          "Surv" = mboost::CoxPH()
        )
      }
      mboost::gamboost(
        formula, data = as.data.frame(formula, data = data),
        na.action = na.pass, weights = weights, family = family,
        baselearner = baselearner, dfbase = dfbase,
        control = mboost::boost_control(...)
      )
    },

    predict = function(object, newdata, .MachineShop, ...) {
      weights <- case_weights(.MachineShop$input)
      newdata <- as.data.frame(newdata)
      if (object$family@name == "Cox Partial Likelihood") {
        lp <- drop(predict(object, type = "link"))
        new_lp <- drop(predict(object, newdata = newdata, type = "link"))
        predict(object$response, lp, new_lp, weights = weights, ...)
      } else {
        predict(object, newdata = newdata, type = "response")
      }
    },

    varimp = function(object, ...) {
      structure(mboost::varimp(object), class = "numeric", metric = "risk")
    }

  )

}

MLModelFunction(GAMBoostModel) <- NULL
