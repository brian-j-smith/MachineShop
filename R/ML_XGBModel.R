#' Extreme Gradient Boosting Models
#'
#' Fits models with an efficient implementation of the gradient boosting
#' framework from Chen & Guestrin.
#'
#' @rdname XGBModel
#'
#' @param nrounds number of boosting iterations.
#' @param ... model parameters as described below and in the XGBoost
#'   \href{https://xgboost.readthedocs.io/en/latest/parameter.html}{documentation}
#'   and arguments passed to \code{XGBModel} from the other constructors.
#' @param verbose numeric value controlling the amount of output printed
#'   during model fitting, such that 0 = none, 1 = performance information, and
#'   2 = additional information.
#' @param print_every_n numeric value designating the fitting iterations at
#'   at which to print output when \code{verbose > 0}.
#' @param eta shrinkage of variable weights at each iteration to prevent
#'   overfitting.
#' @param gamma minimum loss reduction required to split a tree node.
#' @param max_depth maximum tree depth.
#' @param min_child_weight minimum sum of observation weights required of nodes.
#' @param subsample subsample ratio of the training observations.
#' @param colsample_bytree,colsample_bylevel,colsample_bynode subsample ratio of
#'   variables for each tree, level, or split.
#' @param rate_drop rate at which to drop trees during the dropout procedure.
#' @param one_drop integer indicating whether to drop at least one tree during
#'   the dropout procedure.
#' @param skip_drop probability of skipping the dropout procedure during a
#'   boosting iteration.
#' @param alpha,lambda L1 and L2 regularization terms for variable weights.
#' @param max_delta_step,tree_method,sketch_eps,scale_pos_weight,updater,refresh_leaf,process_type,grow_policy,max_leaves,max_bin,num_parallel_tree
#'   other tree booster parameters.
#' @param sample_type,normalize_type type of sampling and normalization
#'   algorithms.
#' @param feature_selector,top_k character string specifying the feature
#'   selection and ordering method, and number of top variables to select in the
#'   \code{"greedy"} and \code{"thrifty"} feature selectors.
#' @param objective optional character string defining the learning task and
#'   objective.  Set automatically if not specified according to the following
#'   values available for supported response variable types.
#'   \describe{
#'     \item{\code{factor}:}{\code{"multi:softprob"}, \code{"binary:logistic"}
#'       (2 levels only)}
#'     \item{\code{numeric}:}{\code{"reg:squarederror"}, \code{"reg:logistic"},
#'       \code{"reg:gamma"}, \code{"reg:tweedie"}, \code{"rank:pairwise"},
#'       \code{"rank:ndcg"}, \code{"rank:map"}}
#'     \item{\code{PoissonVariate}:}{\code{"count:poisson"}}
#'     \item{\code{Surv}:}{\code{"survival:aft"}, \code{"survival:cox"}}
#'   }
#'   The first values listed are the defaults for the corresponding response
#'   types.
#' @param aft_loss_distribution character string specifying a distribution for
#'   the accelerated failure time objective (\code{"survival:aft"}) as
#'   \code{"extreme"}, \code{"logistic"}, or \code{"normal"}.
#' @param aft_loss_distribution_scale numeric scaling parameter for the
#'   accelerated failure time distribution.
#' @param base_score initial prediction score of all observations, global bias.
#'
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric},
#'     \code{PoissonVariate}, \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic Tuning} of Grid Parameters}{
#'     \itemize{
#'       \item XGBDARTModel: \code{nrounds}, \code{eta}*, \code{gamma}*,
#'         \code{max_depth}, \code{min_child_weight}*, \code{subsample}*,
#'         \code{colsample_bytree}*, \code{rate_drop}*, \code{skip_drop}*
#'       \item XGBLinearModel: \code{nrounds}, \code{alpha}, \code{lambda}
#'       \item XGBTreeModel: \code{nrounds}, \code{eta}*, \code{gamma}*,
#'         \code{max_depth}, \code{min_child_weight}*, \code{subsample}*,
#'         \code{colsample_bytree}*
#'     }
#'   }
#' }
#' * excluded from grids by default
#'
#' Default values and further model details can be found in the source link
#' below.
#'
#' In calls to \code{\link{varimp}} for \code{XGBTreeModel}, argument
#' \code{type} may be specified as \code{"Gain"} (default) for the fractional
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
#' varimp(model_fit, method = "model", type = "Frequency", scale = FALSE)
#' }
#'
XGBModel <- function(
  nrounds = 100, ..., objective = character(), aft_loss_distribution = "normal",
  aft_loss_distribution_scale = 1, base_score = 0.5, verbose = 0,
  print_every_n = 1
) {

  params <- as.list(environment())

  MLModel(
    name = "XGBModel",
    label = "Extreme Gradient Boosting",
    packages = "xgboost (>= 1.3.0)",
    response_types = c("factor", "numeric", "PoissonVariate", "Surv"),
    weights = TRUE,
    predictor_encoding = "model.matrix",
    params = new_params(c(params[1], ..., params[-1])),
    fit = function(
      formula, data, weights, nrounds, verbose, print_every_n, ...
    ) {
      x <- model.matrix(data, intercept = FALSE)
      y <- response(data)
      y_levels <- levels(y)

      params <- list(...)
      choices <- switch_class(y,
        "factor" = {
          y <- as.numeric(y) - 1
          c("multi:softprob", if (length(y_levels) <= 2) "binary:logistic")
        },
        "numeric" = c("reg:squarederror", "reg:logistic", "reg:gamma",
                      "reg:tweedie", "rank:pairwise", "rank:ndcg", "rank:map"),
        "PoissonVariate" = "count:poisson",
        "Surv" = {
          throw(check_censoring(y, "right"))
          y_time <- y[, "time"]
          y_event <- y[, "status"] == 1
          c("survival:aft", "survival:cox")
        }
      )
      params$objective <- match.arg(params$objective, choices)

      dmat <- xgboost::xgb.DMatrix(x)
      if (params$objective == "survival:aft") {
        y_lower <- y_upper <- y_time
        y_upper[!y_event] <- Inf
        xgboost::setinfo(dmat, "label_lower_bound", y_lower)
        xgboost::setinfo(dmat, "label_upper_bound", y_upper)
      } else {
        params$aft_loss_distribution <- NULL
        params$aft_loss_distribution_scale <- NULL
        if (params$objective == "survival:cox") {
          y <- ifelse(y_event, y_time, -y_time)
        }
        xgboost::setinfo(dmat, "label", y)
      }

      params$num_class <- if (params$objective == "multi:softprob") {
        length(y_levels)
      }
      scalable <- c("binary:logistic", "reg:logistic", "reg:squarederror")
      if (!(params$objective %in% scalable)) {
        params$scale_pos_weight <- NULL
      }
      if (!isTRUE(params$feature_selector %in% c("greedy", "thrifty"))) {
        params$top_k <- NULL
      }

      (if (verbose) identity else capture.output)(
        model_fit <- xgboost::xgboost(
          dmat, weight = weights, params = params, nrounds = nrounds,
          verbose = verbose, print_every_n = print_every_n
        )
      )
      model_fit$levels <- y_levels
      model_fit
    },
    predict = function(object, newdata, model, times, ...) {
      newx <- model.matrix(newdata, intercept = FALSE)
      xgb_predict <- function(newdata = newx, lp = FALSE) {
        predict(object, newdata = newdata, outputmargin = lp)
      }
      switch(object$params$objective,
        "multi:softprob" = {
          matrix(xgb_predict(), nrow(newx), byrow = TRUE)
        },
        "survival:aft" = {
          distr <- object$params$aft_loss_distribution
          if (distr == "normal") distr <- "gaussian"
          if (length(times)) {
            pred <- xgb_predict(lp = TRUE)
            log_times <- matrix(log(times), length(pred), length(times),
                                byrow = TRUE)
            scale <- object$params$aft_loss_distribution_scale
            quants <- (log_times - pred) / scale
            surv_probs <- switch(distr,
              "extreme" = exp(-exp(quants)),
              "gaussian" = 1 - pnorm(quants),
              "logistic" = 1 / (1 + exp(quants)),
              throw(Error("Unsupported aft loss distribution \"", distr, "\"."))
            )
            SurvProbs(surv_probs, times = times, distr = distr)
          } else {
            SurvTimes(xgb_predict(), distr = distr)
          }
        },
        "survival:cox" = {
          x <- model.matrix(predictor_frame(model), intercept = FALSE)
          lp <- xgb_predict(x, lp = TRUE)
          new_lp <- xgb_predict(newx, lp = TRUE)
          predict(response(model), lp, new_lp, times = times,
                  weights = case_weights(model), ...)
        },
        xgb_predict()
      )
    },
    varimp = function(object, type = c("Gain", "Cover", "Frequency"), ...) {
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
        data.frame(vi[, match.arg(type), drop = FALSE],
                   row.names = vi$Feature)
      }
    }
  )
}

MLModelFunction(XGBModel) <- NULL


#' @rdname XGBModel
#'
XGBDARTModel <- function(
  eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1,
  max_delta_step = .(0.7 * is(y, "PoissonVariate")), subsample = 1,
  colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  alpha = 0, lambda = 1, tree_method = "auto", sketch_eps = 0.03,
  scale_pos_weight = 1, refresh_leaf = 1, process_type = "default",
  grow_policy = "depthwise", max_leaves = 0, max_bin = 256,
  num_parallel_tree = 1, sample_type = "uniform", normalize_type = "tree",
  rate_drop = 0, one_drop = 0, skip_drop = 0, ...
) {
  .XGBModel(name = "XGBDARTModel", label = "Extreme Gradient Boosting (DART)",
            model = "dart", envir = environment(), ...)
}

MLModelFunction(XGBDARTModel) <- NULL


#' @rdname XGBModel
#'
XGBLinearModel <- function(
  alpha = 0, lambda = 0, updater = "shotgun", feature_selector = "cyclic",
  top_k = 0, ...
) {
  .XGBModel(
    name = "XGBLinearModel", label = "Extreme Gradient Boosting (Linear)",
    model = "gblinear", envir = environment(), ...
  )
}

MLModelFunction(XGBLinearModel) <- NULL


#' @rdname XGBModel
#'
XGBTreeModel <- function(
  eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1,
  max_delta_step = .(0.7 * is(y, "PoissonVariate")), subsample = 1,
  colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  alpha = 0, lambda = 1, tree_method = "auto", sketch_eps = 0.03,
  scale_pos_weight = 1, refresh_leaf = 1, process_type = "default",
  grow_policy = "depthwise", max_leaves = 0, max_bin = 256,
  num_parallel_tree = 1, ...
) {
  .XGBModel(name = "XGBTreeModel", label = "Extreme Gradient Boosting (Tree)",
            model = "gbtree", envir = environment(), ...)
}

MLModelFunction(XGBTreeModel) <- NULL


.XGBModel <- function(name, label, model, envir, ...) {
  params <- c(as.list(envir), ...)
  params$booster <- model
  model <- do.call(XGBModel, params, quote = TRUE)
  model@name <- name
  model@label <- label

  gridinfo <- new_gridinfo(
    param = c("nrounds", "eta", "gamma", "max_depth", "min_child_weight",
              "subsample", "colsample_bytree", "rate_drop", "skip_drop",
              "alpha", "lambda"),
    get_values = c(
      function(n, ...) round(seq_range(0, 50, c(1, 1000), n + 1)),
      function(n, ...) seq(0.001, 0.6, length = n),
      function(n, ...) seq(0, 10, length = n),
      function(n, ...) seq_len(min(n, 10)),
      function(n, data, ...) seq(0, min(20, nrow(data)), length = n),
      function(n, ...) seq(0.25, 1, length = n),
      function(n, ...) seq(0.3, 0.8, length = n),
      function(n, ...) seq(0.01, 0.50, length = n),
      function(n, ...) seq(0.05, 0.95, length = n),
      function(n, ...) c(10^-seq_inner(0, 5, n - 1), 0),
      function(n, ...) c(10^-seq_inner(0, 5, n - 1), 0)
    ),
    default = c(TRUE, rep(FALSE, 2), TRUE, rep(FALSE, 5), rep(TRUE, 2))
  )
  grid_params <- switch(params$booster,
    "dart" = c("nrounds", "eta", "gamma", "max_depth", "min_child_weight",
               "subsample", "colsample_bytree", "rate_drop", "skip_drop"),
    "gblinear" = c("nrounds", "alpha", "lambda"),
    "gbtree" = c("nrounds", "eta", "gamma", "max_depth", "min_child_weight",
                 "subsample", "colsample_bytree")
  )
  model@gridinfo <- gridinfo[gridinfo$param %in% grid_params, ]

  model
}
