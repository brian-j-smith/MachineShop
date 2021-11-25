#' Fast Random Forest (SRC) Model
#'
#' Fast OpenMP computing of Breiman's random forest for a variety of data
#' settings including right-censored survival, regression, and classification.
#'
#' @rdname RFSRCModel
#'
#' @param ntree number of trees.
#' @param mtry number of variables randomly selected as candidates for splitting
#'   a node.
#' @param nodesize minumum size of terminal nodes.
#' @param nodedepth maximum depth to which a tree should be grown.
#' @param splitrule splitting rule (see \code{\link[randomForestSRC]{rfsrc}}).
#' @param nsplit non-negative integer value for number of random splits to
#'   consider for each candidate splitting variable.
#' @param block.size interval number of trees at which to compute the cumulative
#'   error rate.
#' @param samptype whether bootstrap sampling is with or without replacement.
#' @param membership logical indicating whether to return terminal node
#'   membership.
#' @param sampsize function specifying the bootstrap size.
#' @param nimpute number of iterations of the missing data imputation algorithm.
#' @param ntime integer number of time points to constrain ensemble calculations
#'   for survival outcomes.
#' @param proximity whether and how to return proximity of cases as measured by
#'   the frequency of sharing the same terminal nodes.
#' @param distance whether and how to return distance between cases as measured
#'   by the ratio of the sum of edges from each case to the root node.
#' @param forest.wt whether and how to return the forest weight matrix.
#' @param xvar.wt vector of non-negative weights representing the probability of
#'   selecting a variable for splitting.
#' @param split.wt vector of non-negative weights used for multiplying the split
#'   statistic for a variable.
#' @param var.used whether and how to return variables used for splitting.
#' @param split.depth whether and how to return minimal depth for each variable.
#' @param do.trace number of seconds between updates to the user on approximate
#'   time to completion.
#' @param statistics logical indicating whether to return split statistics.
#' @param terminal.qualts logical indicating whether to return terminal node
#'   membership information.
#' @param ... arguments passed to \code{RFSRCModel}.
#'
#' @details
#' \describe{
#'   \item{Response types:}{\code{factor}, \code{matrix}, \code{numeric},
#'     \code{Surv}}
#'   \item{\link[=TunedModel]{Automatic tuning} of grid parameters:}{
#'     \code{mtry}, \code{nodesize}
#'   }
#' }
#'
#' Default values and further model details can be found in the source links
#' below.
#'
#' In calls to \code{\link{varimp}} for \code{RFSRCModel}, argument
#' \code{type} may be specified as \code{"permute"} (default) for permutation of
#' OOB cases, as \code{"random"} for permutation replaced with random
#' assignment, or as \code{"anit"} for cases assigned to the split opposite of
#' the random assignments.  Variable importance is automatically scaled to range
#' from 0 to 100.  To obtain unscaled importance values, set
#' \code{scale = FALSE}.  See example below.
#'
#' @return \code{MLModel} class object.
#'
#' @seealso \code{\link[randomForestSRC]{rfsrc}},
#' \code{\link[randomForestSRC]{rfsrc.fast}}, \code{\link{fit}},
#' \code{\link{resample}}
#'
#' @examples
#' \donttest{
#' ## Requires prior installation of suggested package randomForestSRC to run
#'
#' model_fit <- fit(sale_amount ~ ., data = ICHomes, model = RFSRCModel)
#' varimp(model_fit, method = "model", type = "random", scale = TRUE)
#' }
#'
RFSRCModel <- function(
  ntree = 1000, mtry = integer(), nodesize = integer(), nodedepth = integer(),
  splitrule = character(), nsplit = 10, block.size = integer(),
  samptype = c("swor", "swr"), membership = FALSE,
  sampsize = if (samptype == "swor") function(x) 0.632 * x else function(x) x,
  nimpute = 1, ntime = integer(),
  proximity = c(FALSE, TRUE, "inbag", "oob", "all"),
  distance = c(FALSE, TRUE, "inbag", "oob", "all"),
  forest.wt = c(FALSE, TRUE, "inbag", "oob", "all"),
  xvar.wt = numeric(), split.wt = numeric(),
  var.used = c(FALSE, "all.trees", "by.tree"),
  split.depth = c(FALSE, "all.trees", "by.tree"),
  do.trace = FALSE, statistics = FALSE
) {

  samptype <- match.arg(samptype)

  proximity <- match.arg(as.character(proximity), proximity)
  distance <- match.arg(as.character(distance), distance)
  forest.wt <- match.arg(as.character(forest.wt), forest.wt)
  var.used <- match.arg(as.character(var.used), var.used)
  split.depth <- match.arg(as.character(split.depth), split.depth)

  MLModel(

    name = "RFSRCModel",
    label = "Random Forest (SRC)",
    packages = "randomForestSRC",
    response_types = c("factor", "matrix", "numeric", "Surv"),
    weights = TRUE,
    predictor_encoding = "model.frame",
    params = new_params(environment()),

    gridinfo = new_gridinfo(
      param = c("mtry", "nodesize"),
      get_values = c(
        function(n, data, ...) seq_nvars(data, RFSRCModel, n),
        function(n, data, ...) round(seq(1, min(20, nrow(data)), length = n))
      )
    ),

    fit = function(formula, data, weights, ...) {
      y <- response(data)
      data <- as.data.frame(data)
      family <- switch_class(y,
        "matrix" = {
          colnames(y) <- paste0("y", seq_len(ncol(y)))
          "Multivar"
        },
        "Surv" = "Surv"
      )
      if (!is.null(family)) {
        y_names <- map("char", recipes::rand_id, colnames(y))
        data[[response(formula)]] <- NULL
        data[y_names] <- y
        formula[[2]] <- as.call(map(as.name, c(family, y_names)))
      }
      randomForestSRC::rfsrc(formula, data = data, na.action = "na.impute",
                             case.wt = weights, ...)
    },

    predict = function(object, newdata, model, ...) {
      newdata <- as.data.frame(newdata)
      pred <- randomForestSRC::predict.rfsrc(object, newdata = newdata)
      if (pred$family == "regr+") {
        y_names <- pred$yvar.names
        names(y_names) <- sub("_.*", "", y_names)
        simplify(map(function(name) pred$regrOutput[[name]]$predicted, y_names))
      } else if (pred$family == "surv") {
        predict(Surv(pred$time.interest), pred$survival, ...)
      } else {
        pred$predicted
      }
    },

    varimp = function(object, type = c("permute", "random", "anti"), ...) {
      vi <- randomForestSRC::vimp(object, importance = match.arg(type))
      if (vi$family == "regr+") {
        y_names <- vi$yvar.names
        names(y_names) <- sub("_.*", "", y_names)
        simplify(map(function(name) vi$regrOutput[[name]]$importance, y_names))
      } else {
        vi$importance
      }
    }

  )

}

MLModelFunction(RFSRCModel) <- NULL


#' @rdname RFSRCModel
#'
RFSRCFastModel <- function(
  ntree = 500, sampsize = function(x) min(0.632 * x, max(x^0.75, 150)),
  ntime = 50, terminal.qualts = FALSE, ...
) {
  model <- RFSRCModel(ntree = ntree, sampsize = sampsize, ntime = ntime, ...)
  model@params$terminal.qualts <- terminal.qualts
  model
}

MLModelFunction(RFSRCFastModel) <- NULL
