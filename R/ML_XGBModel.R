#' Extreme Gradient Boosting Models
#' 
#' Fits models within an efficient implementation of the gradient boosting
#' framework from Chen & Guestrin.
#' 
#' @rdname XGBModel
#' 
#' @param params list of model parameters as described in the XBoost
#' \href{https://xgboost.readthedocs.io/en/latest/parameter.html}{documentation}.
#' @param nrounds maximum number of boosting iterations.
#' @param verbose numeric value controlling the amount of output printed
#' during model fitting, such that 0 = none, 1 = performance information, and
#' 2 = additional information.
#' @param print_every_n numeric value designating the fitting iterations at
#' at which to print output when \code{verbose > 0}.
#' 
#' @details
#' \describe{
#' \item{Response Types:}{\code{factor}, \code{numeric}}
#' }
#' 
#' Default values for the \code{NULL} arguments and further model details can be
#' found in the source link below.
#' 
#' The following variable importance metrics are available for XBMTreeModel:
#' \enumerate{
#'   \item \code{Gain} fractional contribution of each predictor to the total
#'   gain of its splits.
#'   \item \code{Cover} number of observations related to each predictor.
#'   \item \code{Frequency} percentage of times each predictor is used in the
#'   trees.
#' }
#' In calls to \code{\link{varimp}} for these tree model fits, the first metric
#' will be returned by default.  Others may be specified to the \code{metrics}
#' argument of the function by their names or numeric indices.  Variable
#' importance is automatically scaled to range from 0 to 100.  To obtain
#' unscaled importance values, set \code{scale = FALSE}.  See example below.
#' 
#' @return MLModel class object.
#' 
#' @seealso \code{\link[xgboost:xgb.train]{xgboost}}, \code{\link{fit}},
#' \code{\link{resample}}, \code{\link{tune}}
#'
#' @examples
#' modelfit <- fit(Species ~ ., data = iris, model = XGBTreeModel())
#' varimp(modelfit, metrics = 1:3, scale = FALSE)
#' 
XGBModel <- function(params = list(), nrounds = 1, verbose = 0,
                     print_every_n = 1) {
  MLModel(
    name = "XGBModel",
    packages = "xgboost",
    types = c("factor", "numeric"),
    params = params(environment()),
    nvars = function(data) nvars(data, design = "model.matrix"),
    fit = function(formula, data, weights, params, ...) {
      mf <- model.frame(formula, data, na.action = na.pass)
      x <- model.matrix(formula, mf)[, -1, drop = FALSE]
      y <- model.response(mf)
      if (is.null(params$objective)) params$objective <- 1
      switch_class(y,
                   "factor" = {
                     params$num_class <- nlevels(y)
                     y <- as.numeric(y) - 1
                     obj_choices <- c("multi:softprob", "binary.logistic")
                   },
                   "numeric" = {
                     obj_choices <- c("reg:linear", "reg:logistic", "reg:gamma",
                                      "reg:tweedie", "count:poisson",
                                      "rank:pairwise", "rank:ndcg", "rank:map")
                   })
      params$objective <- match_indices(params$objective, obj_choices)
      mfit <- xgboost::xgboost(x, y, weight = weights, params = params, ...)
      mfit$formula <- formula
      mfit
    },
    predict = function(object, newdata, ...) {
      fo <- object$formula[-2]
      newmf <- model.frame(fo, newdata, na.action = na.pass)
      newx <- model.matrix(fo, newmf)[, -1, drop = FALSE]
      pred <- predict(unMLModelFit(object), newdata = newx)
      if (object$params$objective == "multi:softprob") {
        pred <- matrix(pred, nrow = nrow(newx), byrow = TRUE)
      }
      pred
    },
    varimp = function(object, metrics = 1, ...) {
      vi <- xgboost::xgb.importance(model = object, ...)
      if (!is.null(vi$Weight)) {
        if (!is.null(vi$Class)) {
          y <- fitbit(object, "y")
          vi <- reshape(vi, idvar = "Feature", timevar = "Class",
                        v.names = "Weight", varying = list(levels(y)),
                        direction = "wide")
          data.frame(vi[, -1], row.names = vi$Feature)
        } else {
          structure(vi$Weight, names = vi$Feature)
        }
      } else {
        metrics <- match_indices(metrics, c("Gain", "Cover", "Frequency"))
        data.frame(vi[, metrics, drop = FALSE], row.names = vi$Feature)
      }
    }
  )
}


#' @rdname XGBModel
#' 
#' @param objective character string specifying the learning task and objective.
#' Set automatically according to the class type of the response variable.
#' @param base_score initial numeric prediction score of all instances, global
#' bias.
#' @param eta,gamma,max_depth,min_child_weight,max_delta_step,subsample,colsample_bytree,colsample_bylevel,lambda,alpha,tree_method,sketch_eps,scale_pos_weight,update,refresh_leaf,process_type,grow_policy,max_leaves,max_bin,sample_type,normalize_type,rate_drop,one_drop,skip_drop,updater,feature_selector,top_k
#' see \code{params} reference.
#' @param ... arguments to be passed to \code{XGBModel}.
#' 
XGBDARTModel <- function(objective = NULL, base_score = 0.5,
                         eta = 0.3, gamma = 0, max_depth = 6,
                         min_child_weight = 1, max_delta_step = 0,
                         subsample = 1, colsample_bytree = 1,
                         colsample_bylevel = 1, lambda = 1, alpha = 0,
                         tree_method = "auto", sketch_eps = 0.03,
                         scale_pos_weight = 1, update = "grow_colmaker,prune",
                         refresh_leaf = 1, process_type = "default",
                         grow_policy="depthwise", max_leaves = 0, max_bin = 256,
                         sample_type = "uniform", normalize_type = "tree",
                         rate_drop = 0, one_drop = 0, skip_drop = 0, ...) {
  .XGBModel("dart", environment(), ...)
}


#' @rdname XGBModel
#' 
XGBLinearModel <- function(objective = NULL, base_score = 0.5,
                           lambda = 0, alpha = 0, updater = "shotgun",
                           feature_selector = "cyclic", top_k = 0, ...) {
  .XGBModel("gblinear", environment(), ...)
}


#' @rdname XGBModel
#' 
XGBTreeModel <- function(objective = NULL, base_score = 0.5,
                         eta = 0.3, gamma = 0, max_depth = 6,
                         min_child_weight = 1, max_delta_step = 0,
                         subsample = 1, colsample_bytree = 1,
                         colsample_bylevel = 1, lambda = 1, alpha = 0,
                         tree_method = "auto", sketch_eps = 0.03,
                         scale_pos_weight = 1, update = "grow_colmaker,prune",
                         refresh_leaf = 1, process_type = "default",
                         grow_policy="depthwise", max_leaves = 0, max_bin = 256,
                         ...) {
  .XGBModel("gbtree", environment(), ...)
}


.XGBModel <- function(booster, envir, ...) {
  args <- list(...)
  args$params <- as.call(c(.(list), params(envir), booster = booster))
  do.call(XGBModel, args, quote = TRUE)
}
