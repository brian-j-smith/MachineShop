basehaz <- function(y, risk, times) {
  y_times <- unique(y[,"time"]) %>% sort
  nrisk <- rowsum(risk, y[,"time"]) %>% rev %>% cumsum %>% rev
  nevent <- rowsum(y[,"status"], y[,"time"])[,1]
  cumhaz <- cumsum(nevent / nrisk) %>% structure(names = NULL)
  idx <- approx(y_times, seq(y_times), times, method = "constant",
                f = 0, yleft = 0, yright = length(y_times))$y
  c(0, cumhaz)[idx + 1]
}


params <- function(env) {
  x <- as.list(env)
  missing_args <- names(x)[sapply(x, is.name)]
  if(length(missing_args)) stop("missing values for required argument(s) ",
                                paste0(missing_args, collapse = ", "))
  x[!sapply(x, is.null)]
}
