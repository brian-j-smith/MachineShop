#' Extreme Gradient Boosting Models
#' 
#' Fits models within an efficient implementation of the gradient boosting
#' framework from Chen & Guestrin.
#' 
#' @rdname XGBModel
#' 
#' @param params list of model parameters as described in the XBoost
#'   \href{https://xgboost.readthedocs.io/en/latest/parameter.html}{documentation}.
#' @param nrounds maximum number of boosting iterations.
#' @param verbose numeric value controlling the amount of output printed
#'   during model fitting, such that 0 = none, 1 = performance information, and
#'   2 = additional information.
#' @param print_every_n numeric value designating the fitting iterations at
#'   at which to print output when \code{verbose > 0}.
#' @param objective character string specifying the learning task and objective.
#'   Set automatically according to the class type of the response variable.
#' @param base_score initial numeric prediction score of all instances, global
#'   bias.
#' @param eta,gamma,max_depth,min_child_weight,max_delta_step,subsample,colsample_bytree,colsample_bylevel,lambda,alpha,tree_method,sketch_eps,scale_pos_weight,update,refresh_leaf,process_type,grow_policy,max_leaves,max_bin,sample_type,normalize_type,rate_drop,one_drop,skip_drop,updater,feature_selector,top_k
#'   see \code{params} reference.
#' @param ... arguments passed to \code{XGBModel}.
#' 
#' @details
#' \describe{
#'   \item{Response Types:}{\code{factor}, \code{numeric}}
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
#' \code{metric} may be spedified as \code{"Gain"} (default) for the fractional
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
#' \code{\link{resample}}, \code{\link{tune}}
#'
#' @examples
#' model_fit <- fit(Species ~ ., data = iris, model = XGBTreeModel)
#' varimp(model_fit, metric = "Frequency", scale = FALSE)
#' 
XGBModel <- function(params = list(), nrounds = 1, verbose = 0,
                     print_every_n = 1) {
  
  MLModel(
    name = "XGBModel",
    label = "Extreme Gradient Boosting",
    packages = "xgboost",
    response_types = c("factor", "numeric"),
    predictor_encoding = "model.matrix",
    params = params(environment()),
    fit = function(formula, data, weights, params, ...) {
      x <- model.matrix(data, intercept = FALSE)
      y <- response(data)
      response_levels <- levels(y)
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
      params$objective <- match.arg(params$objective, obj_choices)
      modelfit <- xgboost::xgboost(x, y, weight = weights, params = params, ...)
      modelfit$levels <- response_levels
      modelfit
    },
    predict = function(object, newdata, ...) {
      newx <- model.matrix(newdata, intercept = FALSE)
      pred <- predict(object, newdata = newx)
      if (object$params$objective == "multi:softprob") {
        pred <- matrix(pred, nrow = nrow(newx), byrow = TRUE)
      }
      pred
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
  .XGBModel("XGBDARTModel", "Extreme Gradient Boosting (DART)",
            "dart", environment(), ...)
}

MLModelFunction(XGBDARTModel) <- NULL


#' @rdname XGBModel
#' 
XGBLinearModel <- function(objective = NULL, base_score = 0.5,
                           lambda = 0, alpha = 0, updater = "shotgun",
                           feature_selector = "cyclic", top_k = 0, ...) {
  .XGBModel("XGBLinearModel", "Extreme Gradient Boosting (Linear)",
            "gblinear", environment(), ...)
}

MLModelFunction(XGBLinearModel) <- NULL


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
                   ))
  
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
