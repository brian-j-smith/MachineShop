TrainingParams <- function(
  ..., optim = NULL, control = NULL, cutoff = NULL, stat = NULL
) {
  control <- as.MLControl(control)
  if (is(control, "NullControl")) {
    optim <- NullOptimization()
  } else if (is.null(optim)) {
    optim <- GridSearch()
  }

  if (length(cutoff)) {
    cutoff <- check_numeric(cutoff, bounds = c(0, 1), include = FALSE, size = 1)
    throw(check_assignment(cutoff))
  } else {
    cutoff <- 0.5
  }

  if (length(stat)) {
    stat <- check_stat(stat, convert = TRUE)
    throw(check_assignment(stat))
  } else {
    stat <- function(x) NA_real_
  }

  new("TrainingParams", ..., control = control, optim = optim, cutoff = cutoff,
      stat = stat)
}


TrainingStep <- function(
  object, method = character(), names = character(), items = tibble(),
  params = tibble(), metrics = tibble(), selected = 0,
  performance = Performance()
) {
  nlog <- length(names)
  new("TrainingStep",
    id = object@id,
    name = class(object),
    method = method,
    log = as_tibble(c(
      list(name = names),
      if (is_tibble(items) && nrow(items) == nlog) items,
      list(
        selected = seq_len(nlog) == selected,
        params = as_tibble(params, .rows = nlog),
        metrics = as_tibble(metrics, .rows = nlog)
      )
    )),
    performance = performance
  )
}


#' Training Parameters Monitoring Control
#'
#' Set parameters that control the monitoring of resample estimation of model
#' performance and of tuning parameter optimization.
#'
#' @rdname set_monitor-methods
#'
#' @param object resampling \link[=controls]{control},
#'   tuning parameter \link[=set_optim]{optimization}, or model
#'   \link[=ModelSpecification]{specification} object.
#' @param progress logical indicating whether to display iterative progress
#'   during resampling or optimization.  In the case of resampling, a progress
#'   bar will be displayed if a computing cluster is not registered or is
#'   registered with the \pkg{doSNOW} package.
#' @param verbose numeric or logical value specifying the level of progress
#'   detail to print, with 0 (\code{FALSE}) indicating none and 1 (\code{TRUE})
#'   or higher indicating increasing amounts of detail.
#' @param which character string specifying the monitoring parameters to set as
#'   \code{"all"}, \code{"control"}, or optimization (\code{"optim"}).
#' @param ... arguments passed from the \code{ModelSpecification} method to the
#'   others.
#'
#' @return Argument \code{object} updated with the supplied parameters.
#'
#' @seealso \code{\link{resample}}, \code{\link{set_optim}},
#' \code{\link{set_predict}}, \code{\link{set_strata}}
#'
#' @examples
#' CVControl() %>% set_monitor(verbose = TRUE)
#'
set_monitor <- function(object, ...) {
  UseMethod("set_monitor")
}


set_monitor.list <- function(object, progress, verbose, ...) {
  progress <- check_logical(progress, size = 1)
  object$progress <- throw(check_assignment(progress))

  verbose <- if (is.logical(verbose)) {
    check_logical(verbose, size = 1)
  } else {
    check_integer(verbose, bounds = c(0, Inf), size = 1)
  }
  object$verbose <- throw(check_assignment(verbose))

  object
}


#' @rdname set_monitor-methods
#'
set_monitor.MLControl <- function(
  object, progress = TRUE, verbose = FALSE, ...
) {
  verbose <- check_logical(verbose)
  object@monitor <- set_monitor(object@monitor, progress = progress,
                                verbose = verbose)
  object
}


#' @rdname set_monitor-methods
#'
set_monitor.MLOptimization <- function(
  object, progress = FALSE, verbose = FALSE, ...
) {
  object@monitor <- set_monitor(object@monitor, progress = progress,
                                verbose = verbose)
  object
}


set_monitor.TrainingParams <- function(object, which = "all", ...) {
  if (which == "all") which <- c("control", "optim")
  for (name in which) {
    slot(object, name) <- set_monitor(slot(object, name), ...)
  }
  object
}


#' @rdname set_monitor-methods
#'
set_monitor.ModelSpecification <- function(
  object, which = c("all", "control", "optim"), ...
) {
  object@params <- set_monitor(object@params, which = match.arg(which), ...)
  object
}


#' Tuning Parameter Optimization
#'
#' Set the optimization method and control parameters for tuning of model
#' parameters.
#'
#' @name set_optim
#' @rdname set_optim-methods
#'
#' @param object \link[=inputs]{input} or \link[=models]{model} object.
#' @param num_init number of grid points to sample for the initialization of
#'   Bayesian optimization.
#' @param times maximum number of times to repeat the optimization step.
#'   Multiple sets of model parameters are evaluated automatically at each step
#'   of the BFGS algorithm to compute a finite-difference approximation to the
#'   gradient.
#' @param each number of times to sample and evaluate model parameters at each
#'   optimization step.  This is the swarm size in particle swarm optimization,
#'   which defaults to \code{floor(10 + 2 * sqrt(length(bounds)))}.
#' @param acquisition character string specifying the acquisition function as
#'   \code{"ucb"} (upper confidence bound), \code{"ei"} (expected improvement),
#'   \code{"eips"} (expected improvement per second), or \code{"poi"}
#'   (probability of improvement).
#' @param kappa,conf upper confidence bound (\code{"ucb"}) quantile or its
#'   probability to balance exploitation against exploration.  Argument
#'   \code{kappa} takes precedence if both are given and multiplies the
#'   predictive standard deviation added to the predictive mean in the
#'   acquisition function.  Larger values encourage exploration of the model
#'   parameter space.
#' @param epsilon improvement methods (\code{"ei"}, \code{"eips"}, and
#'   \code{"poi"}) parameter to balance exploitation against exploration.
#'   Values should be between -0.1 and 0.1 with larger ones encouraging
#'   exploration.
#' @param random number of points to sample for a random grid search, or
#'   \code{FALSE} for an exhaustive grid search.  Used when a grid search is
#'   specified or as the fallback method for non-numeric model parameters
#'   present during other optimization methods.
#' @param packages R package or packages to use for the optimization method, or
#'   an empty vector if none are needed.  The first package in
#'   \code{set_optim_bayes} is used unless otherwise specified by the user.
#' @param control list of control parameters passed to
#'   \code{\link[ParBayesianOptimization]{bayesOpt}} by \code{set_optim_bayes}
#'   with package \code{"ParBayesianOptimization"}, to
#'   \code{\link[rBayesianOptimization]{BayesianOptimization}} by
#'   \code{set_optim_bayes} with package \code{"rBayesianOptimization"}, to
#'   \code{\link[stats]{optim}} by \code{set_optim_bfgs} and
#'   \code{set_optim_sann}, and to \code{\link[pso]{psoptim}} by
#'   \code{set_optim_pso}.
#' @param progress logical indicating whether to display iterative progress
#'   during optimization.
#' @param verbose numeric or logical value specifying the level of progress
#'   detail to print, with 0 (\code{FALSE}) indicating none and 1 (\code{TRUE})
#'   or higher indicating increasing amounts of detail.
#' @param fun user-defined optimization function to which the arguments below
#'   are passed in order.  An ellipsis can be included in the function
#'   definition when using only a subset of the arguments and ignoring others.
#'   A tibble returned by the function with the same number of rows as model
#'   evaluations will be included in a \code{TrainingStep} summary of
#'   optimization results; other types of return values will be ignored.
#'   \describe{
#'     \item{optim}{function that takes a numeric vector or list of named
#'       model parameters as the first argument, optionally accepts the maximum
#'       number of iterations as argument \code{max_iter}, and returns a scalar
#'       measure of performance to be maximized.  Parameter names are available
#'       from the \code{grid} and \code{bounds} arguments described below.  If
#'       the function cannot be evaluated at a given set of parameter values,
#'       then \code{-Inf} is returned.}
#'     \item{grid}{data frame containing a tuning grid of all model parameters.}
#'     \item{bounds}{named list of lower and upper bounds for each finite
#'       numeric model parameter in \code{grid}.  The types (integer or double)
#'       of the original parameter values are preserved in the bounds.}
#'     \item{params}{list of optimization parameters as supplied to
#'       \code{set_optim_method}.}
#'     \item{monitor}{list of the \code{progress} and \code{verbose} values.}
#'   }
#' @param label character descriptor for the optimization method.
#' @param params list of user-specified model parameters to be passed to
#'   \code{fun}.
#' @param ... arguments passed to the \code{TrainingParams} method of
#'   \code{set_optim_grid} from its other methods.
#'
#' @details
#' The optimization functions implement the following methods.
#' \describe{
#'   \item{\code{set_optim_bayes}}{Bayesian optimization with a Gaussian process
#'     model (Snoek et al. 2012).}
#'   \item{\code{set_optim_bfgs}}{limited-memory modification of quasi-Newton
#'     BFGS optimization (Byrd et al. 1995).}
#'   \item{\code{set_optim_grid}}{exhaustive or random grid search.}
#'   \item{\code{set_optim_pso}}{particle swarm optimization (Bratton and
#'     Kennedy 2007, Zambrano-Bigiarini et al. 2013).}
#'   \item{\code{set_optim_sann}}{simulated annealing (Belisle 1992).  This
#'     method depends critically on the control parameter settings.  It is
#'     not a general-purpose method but can be very useful in getting to good
#'     parameter values on a very rough optimization surface.}
#'   \item{\code{set_optim_method}}{user-defined optimization function.}
#' }
#' The package-defined optimization functions evaluate and return values of the
#' tuning parameters that are of same type (e.g. integer, double, character) as
#' given in the \code{object} grid.  Sequential optimization of numeric tuning
#' parameters is performed over a hypercube defined by their minimum and maximum
#' grid values.  Non-numeric parameters are optimized with grid searches.
#'
#' @return Argument \code{object} updated with the specified optimization method
#' and control parameters.
#'
#' @references
#' Belisle, C. J. P. (1992). Convergence theorems for a class of simulated
#' annealing algorithms on Rd. \emph{Journal of Applied Probability},
#' \emph{29}, 885–895.
#'
#' Bratton, D. & Kennedy, J. (2007), Defining a standard for particle swarm
#' optimization. In \emph{IEEE Swarm Intelligence Symposium, 2007} (pp.
#' 120-127).
#'
#' Byrd, R. H., Lu, P., Nocedal, J., & Zhu, C. (1995). A limited memory
#' algorithm for bound constrained optimization. \emph{SIAM Journal on
#' Scientific Computing}, \emph{16}, 1190–1208.
#'
#' Snoek, J., Larochelle, H., & Adams, R.P. (2012). Practical Bayesian
#' Optimization of Machine Learning Algorithms. arXiv:1206.2944 [stat.ML].
#'
#' Zambrano-Bigiarini, M., Clerc, M., & Rojas, R. (2013). Standard particle
#' swarm optimisation 2011 at CEC-2013: A baseline for future PSO improvements.
#' In \emph{IEEE Congress on Evolutionary Computation, 2013} (pp. 2337-2344).
#'
#' @seealso \code{\link[rBayesianOptimization]{BayesianOptimization}},
#' \code{\link[ParBayesianOptimization]{bayesOpt}}, \code{\link[stats]{optim}},
#' \code{\link[pso]{psoptim}}, \code{\link{set_monitor}},
#' \code{\link{set_predict}}, \code{\link{set_strata}}
#'
#' @examples
#' ModelSpecification(
#'   sale_amount ~ ., data = ICHomes,
#'   model = TunedModel(GBMModel)
#' ) %>% set_optim_bayes
#'
NULL


#' @rdname set_optim-methods
#'
set_optim_bayes <- function(object, ...) {
  UseMethod("set_optim_bayes")
}


#' @rdname set_optim-methods
#'
set_optim_bayes.ModelSpecification <- function(
  object, num_init = 5, times = 10, each = 1,
  acquisition = c("ucb", "ei", "eips", "poi"), kappa = stats::qnorm(conf),
  conf = 0.995, epsilon = 0, control = list(),
  packages = c("ParBayesianOptimization", "rBayesianOptimization"),
  random = FALSE, progress = verbose, verbose = 0, ...
) {
  if (!is_optim_method(object)) return(object)

  params <- list()

  packages <- match.arg(packages)
  switch(packages,
    "ParBayesianOptimization" = {
      each_upper <- Inf
      verbose_progress <- 2 * !missing(progress)
      fun <- function(optim, bounds, params, monitor, ...) {
        throw(check_optim_bounds(bounds), call = call("set_optim_bayes"))
        num_init <- max(params$num_init, length(bounds) + 1)
        max_iter <- num_init + params$times * params$each
        args <- c(list(
          function(...) {
            score <- optim(list(...), max_iter = max_iter)
            list(Score = max(score, -.Machine$double.xmax))
          },
          bounds = bounds,
          initPoints = num_init,
          iters.n = max_iter - num_init,
          iters.k = params$each,
          acq = params$acquisition,
          kappa = params$kappa,
          eps = params$epsilon,
          verbose = monitor$verbose),
          params$control
        )
        res <- do.call(ParBayesianOptimization::bayesOpt, args)$scoreSummary
        tibble(epoch = as.integer(res$Epoch))
      }
    },
    "rBayesianOptimization" = {
      each_upper <- 1
      verbose_progress <- TRUE && !missing(progress)
      fun <- function(optim, bounds, params, monitor, ...) {
        throw(check_optim_bounds(bounds), call = call("set_optim_bayes"))
        num_init <- max(params$num_init, 2)
        max_iter <- num_init + params$times
        args <- c(list(
          function(...) {
            score <- optim(list(...), max_iter = max_iter)
            list(Score = max(score, -.Machine$double.xmax), Pred = NA)
          },
          bounds = bounds,
          init_points = num_init,
          n_iter = max_iter - num_init,
          acq = switch(params$acquisition,
            "eips" = "ei",
            params$acquisition
          ),
          kappa = params$kappa,
          eps = params$epsilon,
          verbose = as.logical(monitor$verbose)),
          params$control
        )
        do.call(rBayesianOptimization::BayesianOptimization, args)
        tibble(epoch = c(integer(params$num_init), seq_len(params$times)))
      }
    }
  )

  num_init <- check_integer(num_init, bounds = c(1, Inf), size = 1)
  params$num_init <- throw(check_assignment(num_init))

  times <- check_integer(times, bounds = c(1, Inf), size = 1)
  params$times <- throw(check_assignment(times))

  each <- check_integer(each, bounds = c(1, each_upper), size = 1)
  params$each <- throw(check_assignment(each))

  params$acquisition <- match.arg(acquisition)

  if (params$acquisition == "ucb") {
    kappa <- check_numeric(kappa, bounds = c(0, Inf), include = 1:0, size = 1)
    params$kappa <- throw(check_assignment(kappa))
  } else {
    epsilon <- check_numeric(epsilon, include = FALSE, size = 1)
    params$epsilon <- throw(check_assignment(epsilon))
  }

  mloptim <- SequentialOptimization(
    label = "Bayesian Optimization",
    packages = packages,
    params = params,
    fun = fun,
    random = random
  ) %>% set_monitor(progress = progress, verbose = verbose)

  if (verbose_progress && mloptim@monitor$progress) {
    mloptim@monitor$verbose <- verbose_progress
  }

  object@params@optim <- mloptim
  object
}


#' @rdname set_optim-methods
#'
set_optim_bfgs <- function(object, ...) {
  UseMethod("set_optim_bfgs")
}


#' @rdname set_optim-methods
#'
set_optim_bfgs.ModelSpecification <- function(
  object, times = 10, control = list(), random = FALSE, progress = FALSE,
  verbose = 0, ...
) {
  if (!is_optim_method(object)) return(object)

  params <- list()

  times <- check_integer(times, bounds = c(1, Inf), size = 1)
  params$times <- throw(check_assignment(times))

  params$control <- control

  object@params@optim <- SequentialOptimization(
    label = "L-BFGS-B Optimization",
    packages = "stats",
    params = params,
    fun = function(optim, bounds, params, monitor, ...) {
      throw(check_optim_bounds(bounds), call = call("set_optim_bfgs"))
      bounds_mat <- do.call(rbind, bounds)
      lower <- bounds_mat[, 1]
      upper <- bounds_mat[, 2]
      convert <- function(bound, param) {
        if (is.integer(bound)) round_int(param) else param
      }
      stats::optim(
        runif(length(bounds), lower, upper),
        fn = function(x) {
          x <- map("num", convert, bounds, as.list(x))
          min(-optim(x), .Machine$double.xmax)
        },
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        control = within(params$control, {
          trace <- monitor$verbose
          fnscale <- 1
          maxit <- params$times
        })
      )
      return(NULL)
    },
    random = random
  ) %>% set_monitor(progress = progress, verbose = verbose)

  object
}


#' @rdname set_optim-methods
#'
set_optim_grid <- function(object, ...) {
  UseMethod("set_optim_grid")
}


#' @rdname set_optim-methods
#'
set_optim_grid.TrainingParams <- function(
  object, random = FALSE, progress = FALSE, ...
) {
  if (!is_optim_method(object)) return(object)

  object@optim <- if (isFALSE(random)) {
    GridSearch()
  } else {
    RandomGridSearch(size = random)
  }

  object %>% set_monitor(which = "optim", progress = progress)
}


#' @rdname set_optim-methods
#'
set_optim_grid.ModelSpecification <- function(object, ...) {
  object@params <- set_optim_grid(object@params, ...)
  object
}


#' @rdname set_optim-methods
#'
set_optim_grid.TunedInput <- function(object, ...) {
  object@params <- set_optim_grid(object@params, ...)
  object
}


#' @rdname set_optim-methods
#'
set_optim_grid.TunedModel <- function(object, ...) {
  object@params <- set_optim_grid(object@params, ...)
  object
}


#' @rdname set_optim-methods
#'
set_optim_pso <- function(object, ...) {
  UseMethod("set_optim_pso")
}


#' @rdname set_optim-methods
#'
set_optim_pso.ModelSpecification <- function(
  object, times = 10, each = NULL, control = list(), random = FALSE,
  progress = FALSE, verbose = 0, ...
) {
  if (!is_optim_method(object)) return(object)

  params <- list()

  times <- check_integer(times, bounds = c(1, Inf), size = 1)
  params$times <- throw(check_assignment(times))

  if (!is.null(each)) {
    each <- check_integer(each, bounds = c(1, Inf), size = 1)
    params$each <- throw(check_assignment(each))
  }

  control$type <- match.arg(control$type, c("SPSO2007", "SPSO2011"))
  params$control <- control

  object@params@optim <- SequentialOptimization(
    label = "Particle Swarm Optimization",
    packages = "pso",
    params = params,
    fun = function(optim, bounds, params, monitor, ...) {
      throw(check_optim_bounds(bounds), call = call("set_optim_pso"))
      bounds_mat <- do.call(rbind, bounds)
      convert <- function(bound, param) {
        if (is.integer(bound)) round_int(param) else param
      }
      if (is.null(params$each)) {
        params$each <- switch(params$control$type,
          "SPSO2011" = 40,
          floor(10 + 2 * sqrt(length(bounds)))
        )
      }
      max_iter <- params$times * params$each
      res <- pso::psoptim(
        rep(NA, length(bounds)),
        fn = function(x) {
          x <- map("num", convert, bounds, as.list(x))
          min(-optim(x, max_iter = max_iter), .Machine$double.xmax)
        },
        lower = bounds_mat[, 1],
        upper = bounds_mat[, 2],
        control = within(params$control, {
          trace <- monitor$verbose
          fnscale <- 1
          maxit <- params$times
          s <- params$each
        })
      )$counts

      times <- res["iteration"]
      tibble(epoch = rep(seq_len(times), each = res["function"] / times))
    },
    random = random
  ) %>% set_monitor(progress = progress, verbose = verbose)

  object
}


#' @rdname set_optim-methods
#'
set_optim_sann <- function(object, ...) {
  UseMethod("set_optim_sann")
}


#' @rdname set_optim-methods
#'
set_optim_sann.ModelSpecification <- function(
  object, times = 10, control = list(), random = FALSE, progress = FALSE,
  verbose = 0, ...
) {
  if (!is_optim_method(object)) return(object)

  params <- list()

  times <- check_integer(times, bounds = c(1, Inf), size = 1)
  params$times <- throw(check_assignment(times))

  params$control <- control

  object@params@optim <- SequentialOptimization(
    label = "Simulated Annealing",
    packages = "stats",
    params = params,
    fun = function(optim, bounds, params, monitor, ...) {
      throw(check_optim_bounds(bounds), call = call("set_optim_sann"))
      bounds_mat <- do.call(rbind, bounds)
      lower <- bounds_mat[, 1]
      upper <- bounds_mat[, 2]
      convert <- function(bound, param) {
        if (is.integer(bound)) round_int(param) else param
      }
      stats::optim(
        runif(length(bounds), lower, upper),
        fn = function(x) {
          x <- map("num", convert, bounds, as.list(x))
          if (all(x >= lower & x <= upper)) {
            -optim(x, max_iter = params$times)
          } else Inf
        },
        method = "SANN",
        control = within(params$control, {
          trace <- monitor$verbose
          fnscale <- 1
          maxit <- params$times
        })
      )
      return(NULL)
    },
    random = random
  ) %>% set_monitor(progress = progress, verbose = verbose)

  object
}


#' @rdname set_optim-methods
#'
set_optim_method <- function(object, ...) {
  UseMethod("set_optim_method")
}


#' @rdname set_optim-methods
#'
set_optim_method.ModelSpecification <- function(
  object, fun, label = "Optimization Function", packages = character(),
  params = list(), random = FALSE, progress = FALSE, verbose = FALSE, ...
) {
  if (!is_optim_method(object)) return(object)

  object@params@optim <- SequentialOptimization(
    label = label,
    packages = packages,
    params = params,
    fun = function(...) fun(...),
    random = random
  ) %>% set_monitor(progress = progress, verbose = verbose)

  object
}


#' Resampling Prediction Control
#'
#' Set parameters that control prediction during resample estimation of model
#' performance.
#'
#' @param object \link[=controls]{control} object.
#' @param times,distr,method arguments passed to \code{\link{predict}}.
#' @param ... arguments passed to other methods.
#'
#' @return Argument \code{object} updated with the supplied parameters.
#'
#' @seealso \code{\link{resample}}, \code{\link{set_monitor}},
#' \code{\link{set_optim}}, \code{\link{set_strata}}
#'
#' @examples
#' CVControl() %>% set_predict(times = 1:3)
#'
set_predict <- function(
  object, times = numeric(), distr = character(), method = character(), ...
) {
  stopifnot(is(object, "MLControl"))

  predict <- list()

  times <- check_numeric(times, bounds = c(0, Inf), include = FALSE, size = NA,
                         nonempty = FALSE)
  predict$times <- throw(check_assignment(times))

  if (length(distr)) distr <- check_character(distr, size = 1)
  predict$distr <- throw(check_assignment(distr))

  if (length(method)) method <- check_character(method, size = 1)
  predict$method <- throw(check_assignment(method))

  object@predict <- predict
  object
}


#' Resampling Stratification Control
#'
#' Set parameters that control the construction of strata during resample
#' estimation of model performance.
#'
#' @param object \link[=controls]{control} object.
#' @param breaks number of quantile bins desired for stratification of numeric
#'   data during resampling.
#' @param nunique number of unique values at or below which numeric data are
#'   stratified as categorical.
#' @param prop minimum proportion of data in each strata.
#' @param size minimum number of values in each strata.
#' @param ... arguments passed to other methods.
#'
#' @details
#' The arguments control resampling strata which are constructed from numeric
#' proportions for \code{\link{BinomialVariate}}; original values for
#' \code{character}, \code{factor}, \code{logical}, \code{numeric}, and
#' \code{ordered}; first columns of values for \code{matrix}; and numeric times
#' within event statuses for \code{Surv}.  Stratification of survival data by
#' event status only can be achieved by setting \code{breaks = 1}.  Numeric
#' values are stratified into quantile bins and categorical values into factor
#' levels.  The number of bins will be the largest integer less than or equal to
#' \code{breaks} satisfying the \code{prop} and \code{size} control argument
#' thresholds.  Categorical levels below the thresholds will be pooled
#' iteratively by reassigning values in the smallest nominal level to the
#' remaining ones at random and by combining the smallest adjacent ordinal
#' levels.  Missing values are replaced with non-missing values sampled at
#' random with replacement.
#'
#' @return Argument \code{object} updated with the supplied parameters.
#'
#' @seealso \code{\link{resample}}, \code{\link{set_monitor}},
#' \code{\link{set_optim}}, \code{\link{set_predict}}
#'
#' @examples
#' CVControl() %>% set_strata(breaks = 3)
#'
set_strata <- function(
  object, breaks = 4, nunique = 5, prop = 0.1, size = 20, ...
) {
  stopifnot(is(object, "MLControl"))

  strata <- list()

  breaks <- check_integer(breaks, bounds = c(1, Inf), size = 1)
  strata$breaks <- throw(check_assignment(breaks))

  nunique <- check_integer(nunique, bounds = c(1, Inf), size = 1)
  strata$nunique <- throw(check_assignment(nunique))

  prop <- check_numeric(prop, bounds = c(0, 1), size = 1)
  strata$prop <- throw(check_assignment(prop))

  size <- check_integer(size, bounds = c(1, Inf), size = 1)
  strata$size <- throw(check_assignment(size))

  object@strata <- strata
  object
}
