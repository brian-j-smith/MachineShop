.onLoad <- function(libname, pkgname) {
  registerDoSEQ()
  invisible()
}


basehaz <- function(y, risk, times) {
  y_times <- unique(y[,"time"]) %>% sort
  nrisk <- rowsum(risk, y[,"time"]) %>% rev %>% cumsum %>% rev
  nevent <- rowsum(y[,"status"], y[,"time"])[,1]
  cumhaz <- cumsum(nevent / nrisk) %>% structure(names = NULL)
  idx <- approx(y_times, seq(y_times), times, method = "constant",
                f = 0, yleft = 0, yright = length(y_times))$y
  c(0, cumhaz)[idx + 1]
}


field <- function(object, name) {
  if(isS4(object)) slot(object, name) else object[[name]]
}

"field<-" <- function(object, name, value) {
  if(isS4(object)) slot(object, name) <- value else object[[name]] <- value
  object
}


match.indices <- function(indices, choices) {
  lookup <- structure(seq(choices), names = choices)
  indices <- na.omit(names(lookup)[lookup[indices]])
  if(length(indices) == 0) {
    indices <- names(lookup)[1]
    warning("specified indices not found; using ", indices, " instead")
  }
  indices
}


params <- function(env) {
  x <- as.list(env)
  missing_args <- names(x)[sapply(x, is.name)]
  if(length(missing_args)) stop("missing values for required argument(s) ",
                                paste0(missing_args, collapse = ", "))
  x[!sapply(x, is.null)]
}


requireModelNamespaces <- function(packages) {
  pass <- sapply(packages, requireNamespace)
  if(!all(pass)) stop("install required packages: ",
                      paste(packages[!pass], collapse = ", "))
  invisible(pass)
}


strata <- function(object, ...) {
  UseMethod("strata", object)
}


strata.default <- function(object, ...) {
  object
}


strata.Surv <- function(object, ...) {
  object[, "status"]
}
