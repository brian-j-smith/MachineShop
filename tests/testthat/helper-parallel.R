with_parallel <- function(code) {
  cores <- as.numeric(Sys.getenv("TEST_CORES"))
  if (is.na(cores)) cores <- 1
  if (require("doSNOW", quietly = TRUE) && cores > 1) {
    cl <- makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
    on.exit(stopCluster(cl))
  }

  code
}
