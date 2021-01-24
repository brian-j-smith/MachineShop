#' Extreme Gradient Boosting Models
#'
#' Fits models within an efficient implementation of the gradient boosting
#' framework from Chen & Guestrin.
#'
#' @rdname XGBModel
#'
#' @param params list of model parameters as described in the XGBoost
#'   \href{https://xgboost.readthedocs.io/en/latest/parameter.html}{documentation}.
#' @param nrounds maximum number of boosting iterations.
#' @param verbose numeric value controlling the amount of output printed
#'   during model fitting, such that 0 = none, 1 = performance information, and
#'   2 = additional information.
#' @param print_every_n numeric value designating the fitting iterations at
#'   at which to print output when \code{verbose > 0}.
#' @param objective character string specifying the learning task and objective.
#'   Possible values for supported response variable types are as follows.
#'   \describe{
#'     \item{\code{factor}:}{\code{"multi:softprob"}, \code{"binary:logistic"}
#'       (2 levels only)}
#'     \item{\code{numeric}:}{\code{"reg:squarederror"}, \code{"reg:logistic"},
#'       \code{"reg:gamma"}, \code{"reg:tweedie"}, \code{"rank:pairwise"},
#'       \code{"rank:ndcg"}, \code{"rank:map"}}
#'     \item{\code{PoissonVariate}:}{\code{"count:poisson"}}
#'     \item{\code{Surv}:}{\code{"survival:cox"}, \code{"survival:aft"}}
#'   }
#'   The first values listed are the defaults for the corresponding response
#'   types.
#' @param aft_loss_distribution character string specifying the distribution for
#'   the accelerated failure time objective (\code{"survival:aft"}) as
#'   \code{"normal"}, \code{"logistic"}, or \code{"extreme"}.
#' @param aft_loss_distribution_scale numeric scaling parameter for the
#'   accelerated failure time distribution.
#' @param base_score initial numeric prediction score of all instances, global
#'   bias.
#' @param eta,gamma,max_depth,min_child_weight,max_delta_step,subsample,colsample_bytree,colsample_bylevel,colsample_bynode,lambda,alpha,tree_method,sketch_eps,scale_pos_weight,refresh_leaf,process_type,grow_policy,max_leaves,max_bin,num_parallel_tree,sample_type,normalize_type,rate_drop,one_drop,skip_drop,updater,feature_selector,top_k
#'   see \code{params} reference.
#' @param ... arguments passed to \code{XGBModel}.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric},
#'     \code{PoissonVariate}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters}{
#'     \itemize{
#'       \item XGBDARTModel: \code{nrounds}, \code{max_depth}, \code{eta},
#'         \code{gamma}*, \code{min_child_weight}*, \code{subsample},
#'         \code{colsample_bytree}, \code{rate_drop}, \code{skip_drop}
#'       \item XGBLinearModel: \code{nrounds}, \code{lambda}, \code{alpha}
#'       \item XGBTreeModel: \code{nrounds}, \code{max_depth}, \code{eta},
#'         \code{gamma}*, \code{min_child_weight}*, \code{subsample},
#'         \code{colsample_bytree}
#'     }
#'   }
#' }
#' * included only in randomly sampled grid points
#'
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#'
#' In calls to \code{\link{varimp}} for \code{XGBTreeModel}, argument
#' \code{metric} may be specified as \code{"Gain"} (default) for the fractional
#' contribution of each predictor to the total gain of its splits, as
#' \code{"Cover"} for the number of observations related to each predictor, or
#' as \code{"Frequency"} for the percentage of times each predictor is used in
#' the trees.  Variable importance is automatically scaled to range from 0 to
#' 100.  To obtain unscaled importance values, set \code{scale = FALSE}.  See
#' example below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[xgboost:xgb.train]{xgboost}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package xgboost to run
#'
#' model_fit <- fit(Species ~ ., data = iris, model = XGBTreeModel)
#' varimp(model_fit, metric = "Frequency", scale = FALSE)
#' }
#'
XGBModel <- function(params = list(), nrounds = 1, verbose = 0,
                     print_every_n = 1) {

  MLModel(
    name = "XGBModel",
    label = "Extreme Gradient Boosting",
    packages = "xgboost",
    response_types = c("factor", "numeric", "PoissonVariate", "Surv"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    fit = function(formula, data, weights, params, ...) {
      x <- model.matrix(data, intercept = FALSE)
      y <- response(data)
      response_levels <- levels(y)

      obj_choices <- switch_class(y,
        "factor" = {
          y <- as.numeric(y) - 1
          c("multi:softprob",
            if (length(response_levels) <= 2) "binary:logistic")
        },
        "numeric" = c("reg:squarederror", "reg:logistic", "reg:gamma",
                      "reg:tweedie", "rank:pairwise", "rank:ndcg", "rank:map"),
        "PoissonVariate" = "count:poisson",
        "Surv" = {
          y_time <- y[, "time"]
          y_event <- y[, "status"] != 0
          c("survival:cox", "survival:aft")
        }
      )
      params$objective <- match.arg(params$objective, obj_choices)

      dmat <- xgboost::xgb.DMatrix(x)
      if (params$objective == "survival:aft") {
        y_lower <- y_upper <- y_time
        y_upper[!y_event] <- Inf
        xgboost::setinfo(dmat, "label_lower_bound", y_lower)
        xgboost::setinfo(dmat, "label_upper_bound", y_upper)
      } else {
        params$aft_loss_distribution <- NULL
        params$aft_loss_distribution_scale <- NULL
        if (params$objective == "survival:cox") y <- y_time * (2 * y_event - 1)
        xgboost::setinfo(dmat, "label", y)
      }

      params$num_class <- if (params$objective == "multi:softprob") {
        length(response_levels)
      }
      if (!(params$objective %in% c("binary:logistic",
                                    "reg:logistic",
                                    "reg:squarederror"))) {
        params$scale_pos_weight <- NULL
      }
      if (!isTRUE(params$feature_selector %in% c("greedy", "thrifty"))) {
        params$top_k <- NULL
      }

      modelfit <- xgboost::xgboost(dmat, weight = weights, params = params, ...)
      modelfit$levels <- response_levels
      modelfit
    },
    predict = function(object, newdata, model, times, ...) {
      newx <- model.matrix(newdata, intercept = FALSE)
      pred <- predict(object, newdata = newx)
      switch(object$params$objective,
        "multi:softprob" = matrix(pred, nrow = nrow(newx), byrow = TRUE),
        "survival:aft" = if (is.null(times)) {
          pred
        } else {
          stop("time-specific prediction not available for XGBModel",
               " survival:aft")
        },
        "survival:cox" = {
          x <- model.matrix(predictor_frame(model), intercept = FALSE)
          lp <- log(predict(object, newdata = x))
          new_lp <- log(pred)
          predict(response(model), lp, times, new_lp, ...)
        },
        pred
      )
    },
    varimp = function(object, metric = c("Gain", "Cover", "Frequency"), ...) {
      vi <- xgboost::xgb.importance(model = object, ...)
      if (!is.null(vi$Weight)) {
        if (!is.null(vi$Class)) {
          vi <- reshape(vi, idvar = "Feature", timevar = "Class",
                        v.names = "Weight", varying = list(object$levels),
                        direction = "wide")
          data.frame(vi[, -1], row.names = vi$Feature)
        } else {
          structure(vi$Weight, names = vi$Feature)
        }
      } else {
        data.frame(vi[, match.arg(metric), drop = FALSE],
                   row.names = vi$Feature)
      }
    }
  )

}

MLModelFunction(XGBModel) <- NULL


#' @rdname XGBModel
#'
XGBDARTModel <- function(objective = NULL, aft_loss_distribution = "normal",
                         aft_loss_distribution_scale = 1, base_score = 0.5,
                         eta = 0.3, gamma = 0, max_depth = 6,
                         min_child_weight = 1,
                         max_delta_step = .(0.7 * is(y, "PoissonVariate")),
                         subsample = 1, colsample_bytree = 1,
                         colsample_bylevel = 1, colsample_bynode = 1,
                         lambda = 1, alpha = 0, tree_method = "auto",
                         sketch_eps = 0.03, scale_pos_weight = 1,
                         refresh_leaf = 1, process_type = "default",
                         grow_policy = "depthwise", max_leaves = 0,
                         max_bin = 256, num_parallel_tree = 1,
                         sample_type = "uniform", normalize_type = "tree",
                         rate_drop = 0, one_drop = 0, skip_drop = 0, ...) {
  .XGBModel("XGBDARTModel", "Extreme Gradient Boosting (DART)",
            "dart", environment(), ...)
}

MLModelFunction(XGBDARTModel) <- NULL


#' @rdname XGBModel
#'
XGBLinearModel <- function(objective = NULL, aft_loss_distribution = "normal",
                           aft_loss_distribution_scale = 1, base_score = 0.5,
                           lambda = 0, alpha = 0, updater = "shotgun",
                           feature_selector = "cyclic", top_k = 0, ...) {
  .XGBModel("XGBLinearModel", "Extreme Gradient Boosting (Linear)",
            "gblinear", environment(), ...)
}

MLModelFunction(XGBLinearModel) <- NULL


#' @rdname XGBModel
#'
XGBTreeModel <- function(objective = NULL, aft_loss_distribution = "normal",
                         aft_loss_distribution_scale = 1, base_score = 0.5,
                         eta = 0.3, gamma = 0, max_depth = 6,
                         min_child_weight = 1,
                         max_delta_step = .(0.7 * is(y, "PoissonVariate")),
                         subsample = 1, colsample_bytree = 1,
                         colsample_bylevel = 1, colsample_bynode = 1,
                         lambda = 1, alpha = 0, tree_method = "auto",
                         sketch_eps = 0.03, scale_pos_weight = 1,
                         refresh_leaf = 1, process_type = "default",
                         grow_policy = "depthwise", max_leaves = 0,
                         max_bin = 256, num_parallel_tree = 1, ...) {
  .XGBModel("XGBTreeModel", "Extreme Gradient Boosting (Tree)",
            "gbtree", environment(), ...)
}

MLModelFunction(XGBTreeModel) <- NULL


.XGBModel <- function(name, label, booster, envir, ...) {
  args <- list(...)
  args$params <- as.call(c(.(list), params(envir), booster = booster))
  model <- do.call(XGBModel, args, quote = TRUE)
  model@name <- name
  model@label <- label

  params <- switch(booster,
    "dart" = list(
      nrounds = NULL,
      max_depth = NULL,
      eta = NULL,
      gamma = NULL,
      min_child_weight = NULL,
      subsample = NULL,
      colsample_bytree = NULL,
      rate_drop = NULL,
      skip_drop = NULL
    ),
    "gblinear" = list(
      nrounds = NULL,
      lambda = NULL,
      alpha = NULL
    ),
    "gbtree" = list(
      nrounds = NULL,
      max_depth = NULL,
      eta = NULL,
      gamma = NULL,
      min_child_weight = NULL,
      subsample = NULL,
      colsample_bytree = NULL
    )
  )

  if (length(params)) {
    model@grid <- function(x, length, random, ...) {
      params <- params %>%
        set_param("nrounds",
                  round(seq_range(0, 50, c(1, 1000), length + 1))) %>%
        set_param("max_depth", 1:min(length, 10)) %>%
        set_param("eta", c(0.3, 0.4)) %>%
        set_param("subsample", seq(0.25, 1, length = length)) %>%
        set_param("colsample_bytree", c(0.6, 0.8)) %>%
        set_param("rate_drop", c(0.01, 0.50)) %>%
        set_param("skip_drop", c(0.05, 0.95)) %>%
        set_param("lambda", c(10^-seq_inner(0, 5, length - 1), 0)) %>%
        set_param("alpha", c(10^-seq_inner(0, 5, length - 1), 0))

      if (random) {
        params <- params %>%
          set_param("eta", seq(0.001, 0.6, length = length)) %>%
          set_param("gamma", seq(0, 10, length = length)) %>%
          set_param("min_child_weight",
                    seq(0, min(20, nrow(x)), length = length)) %>%
          set_param("colsample_bytree", seq(0.3, 0.8, length = length)) %>%
          set_param("rate_drop", seq(0.01, 0.50, length = length)) %>%
          set_param("skip_drop", seq(0.05, 0.95, length = length))
      }

      params
    }
  }

  model
}
